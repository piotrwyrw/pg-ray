-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The Unhinged PL/pgSQL Ray-Tracing Engine v2.0
--
-- (C) 2026 Piotr K. Wyrwwas, Michael T. Steinmötzger
-- This work is licensed under GPL-3.0
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

drop schema if exists pgray cascade;
create schema pgray;
set search_path to pgray;

-- PL/pgSQL loves to fail silently and mess with my code. Let's do something about it.
set plpgsql.extra_errors = 'all';

create domain id_t as bigint;

-- Used to store settings (since global variables are not a thing here)
create unlogged table property
(
    key   text primary key not null,
    value bigint           not null
);

create unlogged table pixel
(
    id bigint primary key generated always as identity not null,
    r  integer check (r >= 0 and r <= 255)             not null,
    g  integer check (g >= 0 and g <= 255)             not null,
    b  integer check (b >= 0 and b <= 255)             not null
);

create unlogged table vec3
(
    id bigint primary key generated always as identity not null,
    x  double precision                                not null,
    y  double precision                                not null,
    z  double precision                                not null
);

create unlogged table color
(
    id bigint primary key generated always as identity not null,
    r  integer                                         not null,
    g  integer                                         not null,
    b  integer                                         not null
);

create unlogged table environment
(
    id                       bigint primary key generated always as identity not null,
    sun_direction_vec3_id    bigint                                          not null,
    ambient_light_intensity  double precision                                not null,
    camera_direction_vec3_id bigint                                          not null,
    camera_location_vec3_id  bigint                                          not null,
    view_plane_size          double precision                                not null,

    foreign key (sun_direction_vec3_id) references vec3 (id),
    foreign key (camera_direction_vec3_id) references vec3 (id),
    foreign key (camera_location_vec3_id) references vec3 (id)
);

create type shader_type as enum ( 'diffuse', 'mirror' );

create unlogged table sphere
(
    id               bigint primary key generated always as identity not null,
    color_id         bigint                                          not null,
    location_vec3_id bigint                                          not null,
    radius           double precision                                not null,
    shader           shader_type                                     not null,

    foreign key (color_id) references color (id),
    foreign key (location_vec3_id) references vec3 (id)
);

create unlogged table ray
(
    id                bigint primary key generated always as identity not null,
    origin_vec3_id    bigint                                          not null,
    direction_vec3_id bigint                                          not null,

    foreign key (origin_vec3_id) references vec3 (id),
    foreign key (direction_vec3_id) references vec3 (id)
);

create unlogged table intersection
(
    id            bigint primary key generated always as identity not null,
    ray_id        bigint                                          not null,
    sphere_id     bigint                                          not null,
    point_vec3_id bigint                                          not null,
    distance      double precision                                not null,

    foreign key (ray_id) references ray (id),
    foreign key (sphere_id) references sphere (id),
    foreign key (point_vec3_id) references vec3 (id)
);

create unlogged table render_output
(
    id          bigint primary key generated always as identity not null,
    rendered_at timestamptz                                     not null,
    ppm         text                                            not null
);

create unlogged table color_accumulator
(
    r integer check (r >= 0 and r <= 255) not null,
    g integer check (g >= 0 and g <= 255) not null,
    b integer check (b >= 0 and b <= 255) not null
);

create unlogged table bounces
(
    id bigint primary key generated always as identity not null
);


--   ##############################
--     Bounce Tracking Functions
--   ##############################

create or replace function bounces_increment() returns void as
$$
begin
    insert into bounces default values;
end;
$$ language plpgsql;

create or replace function bounces_count() returns bigint as
$$
begin
    return (select count(*) from bounces);
end;
$$ language plpgsql;

create or replace function bounces_clear() returns void as
$$
begin
    delete from bounces;
end;
$$ language plpgsql;


--   ###############################
--     Color Accumulator Functions
--   ###############################

create or replace function accumulator_sample(_r integer, _g integer, _b integer) returns void as
$$
begin
    insert into color_accumulator (r, g, b) values (_r, _g, _b);
end;
$$ language plpgsql;

create or replace function accumulator_clear() returns void as
$$
begin
    delete from color_accumulator;
end;
$$ language plpgsql;

create or replace function accumulator_commit(clear boolean default true) returns void as
$$
declare
    final_r integer := 0;
    final_g integer := 0;
    final_b integer := 0;
begin
    if (select count(*) from color_accumulator) = 0 then
        insert into color_accumulator (r, g, b) values (0, 0, 0);
    end if;

    select sum(ca.r) / count(*) from color_accumulator ca into final_r;
    select sum(ca.g) / count(*) from color_accumulator ca into final_g;
    select sum(ca.b) / count(*) from color_accumulator ca into final_b;
    insert into pixel (r, g, b) values (final_r, final_g, final_b);

    if clear then
        perform accumulator_clear();
    end if;
end;
$$ language plpgsql;


--   ######################
--     Property Functions
--   ######################

create or replace function property_set(key text, value bigint) returns void as
$$
declare
    mapKey   constant text   := key;
    mapValue constant bigint := value;
begin
    if exists (select * from property where property.key = mapKey) then
        update property set value = mapValue where key = mapKey;
        return;
    end if;

    insert into property values (key, value);
end;
$$ language plpgsql;

create or replace function property_get(key text) returns text as
$$
declare
    mapKey constant text   := key;
    result          bigint := null;
begin
    select property.value as mapKey from property where property.key = mapKey into result;
    if result is null then
        raise exception 'Attempting to access undefined variable "%"!', key;
    end if;
    return result;
end;
$$ language plpgsql;

create or replace function property_width() returns integer as
$$
begin
    return property_get('width')::integer;
end;
$$ language plpgsql;

create or replace function property_height() returns integer as
$$
begin
    return property_get('height')::integer;
end;
$$ language plpgsql;

create or replace function property_environment_id() returns id_t as
$$
begin
    return property_get('environment')::id_t;
end;
$$ language plpgsql;

create or replace function property_set_dimensions(width integer, height integer) returns void as
$$
begin
    perform property_set('width', width);
    perform property_set('height', height);
end;
$$ language plpgsql;

create or replace function property_set_environment(environment_id id_t) returns void as
$$
begin
    perform property_set('environment', environment_id);
end;
$$ language plpgsql;


--   #####################
--     Utility Functions
--   #####################

-- Converts an index to a cartesian (x, y) coordinate
create or replace function itoxy(i integer, x out integer, y out integer) returns record as
$$
declare
    width integer := (select property_get('width'));
begin
    select i / width, i % width into y, x;
end;
$$ language plpgsql;

-- Converts a (x, y) coordinate to a pixel index
create or replace function xytoi(x integer, y integer) returns integer as
$$
declare
    width integer := (select property_get('width'));
begin
    return (y * width) + x;
end ;
$$ language plpgsql;

create or replace function clamp_pixel_value(n double precision) returns double precision as
$$
begin
    if n < 0 then
        return 0;
    elsif n > 255 then
        return 255;
    else
        return n;
    end if;
end;
$$ language plpgsql;

create or replace function max(n1 double precision, n2 double precision) returns double precision as
$$
begin
    if n1 > n2 then
        return n1;
    elsif n2 > n1 then
        return n2;
    else
        return n1;
    end if;
end;
$$ language plpgsql;

create or replace function min(n1 double precision, n2 double precision) returns double precision as
$$
begin
    if n1 < n2 then
        return n1;
    elsif n2 < n1 then
        return n2;
    else
        return n1;
    end if;
end;
$$ language plpgsql;

create or replace function format_interval_between(start_timestamp timestamptz, end_timestamp timestamptz) returns text as
$$
begin
    return format_interval(end_timestamp - start_timestamp);
end;
$$ language plpgsql;

create or replace function format_interval(_interval interval) returns text as
$$
declare
    total_ms  int  := round(extract(epoch from _interval) * 1000);
    hours     int;
    minutes   int;
    seconds   int;
    millis    int;
    formatted text := '';
begin
    hours := (total_ms / (1000 * 60 * 60))::int;
    minutes := ((total_ms / (1000 * 60))::int % 60);
    seconds := ((total_ms / 1000)::int % 60);
    millis := (total_ms::int % 1000);

    if hours > 0 then
        formatted := formatted || format('%s hours ', hours);
    end if;

    if minutes > 0 then
        formatted := formatted || format('%s minutes ', minutes);
    end if;

    if seconds > 0 then
        formatted := formatted || format('%s seconds ', seconds);
    end if;

    formatted := formatted || format('%s ms', millis);
    return formatted;
end;
$$ language plpgsql;

--   ####################
--     Vector Functions
--   ####################

create or replace function vec_new(_x double precision, _y double precision, _z double precision) returns id_t as
$$
declare
    vec3_id id_t;
begin
    insert into vec3 (x, y, z) values (_x, _y, _z) returning id into vec3_id;
    return vec3_id;
end;
$$ language plpgsql;

create or replace function vec_copy(vec3_id id_t) returns id_t as
$$
declare
    new_vec3_id id_t;
begin
    insert into vec3 (x, y, z) select v.x, v.y, v.z from vec3 v where v.id = vec3_id returning id into new_vec3_id;
    return new_vec3_id;
end;
$$ language plpgsql;

create or replace function vec_dot(a_vec3_id id_t, b_vec3_id id_t) returns double precision as
$$
declare
    a_vec3 vec3%rowType;
    b_vec3 vec3%rowType;
begin
    select * from vec3 where id = a_vec3_id into a_vec3;
    select * from vec3 where id = b_vec3_id into b_vec3;
    return a_vec3.x * b_vec3.x + a_vec3.y * b_vec3.y + a_vec3.z * b_vec3.z;
end;
$$ language plpgsql;

create or replace function vec_cross(a_vec3_id id_t, b_vec3_id id_t) returns id_t as
$$
declare
    a_vec3  vec3%rowtype;
    b_vec3  vec3%rowtype;
    cross_x double precision;
    cross_y double precision;
    cross_z double precision;
begin
    select v.* into a_vec3 from vec3 v where v.id = a_vec3_id;
    select v.* into b_vec3 from vec3 v where v.id = b_vec3_id;

    cross_x := a_vec3.y * b_vec3.z - a_vec3.z * b_vec3.y;
    cross_y := a_vec3.z * b_vec3.x - a_vec3.x * b_vec3.z;
    cross_z := a_vec3.x * b_vec3.y - a_vec3.y * b_vec3.x;

    return vec_new(cross_x, cross_y, cross_z);
end;
$$ language plpgsql;

create or replace function vec_magnitude(vec3_id id_t) returns double precision as
$$
declare
    v vec3%rowType;
begin
    select * from vec3 where id = vec3_id into v;
    return sqrt(v.x ^ 2 + v.y ^ 2 + v.z ^ 2);
end;
$$ language plpgsql;

create or replace function vec_normalize(vec3_id id_t) returns id_t as
$$
declare
    vec_row   vec3%rowType;
    magnitude double precision = (select vec_magnitude(vec3_id));
begin
    if magnitude = 0 then
        return vec_copy(vec3_id);
    end if;

    select id, x, y, z
    from vec3
    where id = vec3_id
    into vec_row;

    return vec_new(vec_row.x / magnitude, vec_row.y / magnitude, vec_row.z / magnitude);
end;
$$ language plpgsql;

create or replace function vec_sub(a_vec3_id id_t, b_vec3_id id_t) returns id_t as
$$
declare
    vec_a vec3%rowType;
    vec_b vec3%rowType;
begin
    select v.* from vec3 v where id = a_vec3_id into vec_a;
    select v.* from vec3 v where id = b_vec3_id into vec_b;
    return vec_new(vec_a.x - vec_b.x, vec_a.y - vec_b.y, vec_a.z - vec_b.z);
end;
$$ language plpgsql;

create or replace function vec_add(a_vec3_id id_t, b_vec3_id id_t) returns id_t as
$$
declare
    vec_a vec3%rowType;
    vec_b vec3%rowType;
begin
    select v.* from vec3 v where id = a_vec3_id into vec_a;
    select v.* from vec3 v where id = b_vec3_id into vec_b;
    return vec_new(vec_a.x + vec_b.x, vec_a.y + vec_b.y, vec_a.z + vec_b.z);
end;
$$ language plpgsql;

create or replace function vec_add_random(vec3_id id_t, amount double precision) returns id_t as
$$
begin
    return vec_add(vec3_id, vec_new(random() * amount, random() * amount, random() * amount));
end;
$$ language plpgsql;

create or replace function vec_multiply(vec3_id id_t, fac double precision) returns id_t as
$$
declare
    _vec3 vec3%rowType;
begin
    select v.* from vec3 v where id = vec3_id into _vec3;
    return vec_new(_vec3.x * fac, _vec3.y * fac, _vec3.z * fac);
end;
$$ language plpgsql;

create or replace function vec_flip(vec3_id id_t) returns id_t as
$$
declare
    _vec3 vec3%rowtype;
begin
    select v.* from vec3 v where id = vec3_id into _vec3;
    return vec_new(-_vec3.x, -_vec3.y, -_vec3.z);
end;
$$ language plpgsql;

create or replace function vec_reflect(incident_vec3_id id_t, normal_vec3_id id_t) returns id_t as
$$
declare
    reflected_vec3_id          id_t;
    projection_incident_normal double precision;
    scaled_normal_vec3_id      id_t;
begin
    scaled_normal_vec3_id := vec_copy(normal_vec3_id);
    projection_incident_normal := vec_dot(incident_vec3_id, normal_vec3_id) * 2;
    scaled_normal_vec3_id := vec_multiply(scaled_normal_vec3_id, projection_incident_normal);
    reflected_vec3_id = vec_copy(incident_vec3_id);
    reflected_vec3_id := vec_sub(incident_vec3_id, scaled_normal_vec3_id);
    return reflected_vec3_id;
end;
$$ language plpgsql;


--   ###################
--     Color Functions
--   ###################

create or replace function color_new(_r integer, _g integer, _b integer) returns id_t as
$$
declare
    color_id id_t;
begin
    insert into color (r, g, b)
    values (_r, _g, _b)
    returning id
        into color_id;
    return color_id;
end;
$$ language plpgsql;


--   #################
--     Ray Functions
--   #################

create or replace function ray_new_plane(_origin_vec3_id id_t, plane_location_vec3_id id_t) returns id_t as
$$
declare
    ray_direction_vec3_id id_t;
    ray_id                id_t;
begin
    ray_direction_vec3_id := vec_sub(plane_location_vec3_id, _origin_vec3_id);
    ray_direction_vec3_id := vec_normalize(ray_direction_vec3_id);

    insert into ray (origin_vec3_id, direction_vec3_id)
    values (_origin_vec3_id, ray_direction_vec3_id)
    returning id
        into ray_id;

    return ray_id;
end;
$$ language plpgsql;

create or replace function ray_new(_origin_vec3_id id_t, _direction_vec3_id id_t) returns id_t as
$$
declare
    ray_id id_t;
begin
    insert into ray (origin_vec3_id, direction_vec3_id)
    values (_origin_vec3_id, _direction_vec3_id)
    returning id
        into ray_id;

    return ray_id;
end;
$$ language plpgsql;


--   ####################
--     Ray-Tracing Math
--   ####################

create or replace function ray_sphere(ray_id id_t, sphere_id id_t) returns id_t as
$$
declare
    epsilon               double precision := 10e-6;
    tmp_select_record     record;
    _ray                  ray%rowType;
    _sphere               sphere%rowType;
    dir_vec3              vec3%rowType;
    origin_vec3           vec3%rowType;
    sphere_location_vec3  vec3%rowType;
    Dx                    double precision;
    Dy                    double precision;
    Dz                    double precision;
    Cx                    double precision;
    Cy                    double precision;
    Cz                    double precision;
    Ox                    double precision;
    Oy                    double precision;
    Oz                    double precision;
    radius                double precision;
    quad_A                double precision;
    quad_B                double precision;
    quad_C                double precision;
    discriminant          double precision;
    t1                    double precision;
    t2                    double precision;
    t                     double precision;
    intersect_x           double precision;
    intersect_y           double precision;
    intersect_z           double precision;
    intersection_vec3d_id id_t;
    intersection_id       id_t;
begin
    select r, s, dir, org, loc
    into tmp_select_record
    from ray r
             join sphere s on s.id = sphere_id
             join vec3 dir on dir.id = r.direction_vec3_id
             join vec3 org on org.id = r.origin_vec3_id
             join vec3 loc on loc.id = s.location_vec3_id
    where r.id = ray_id;

    _ray := tmp_select_record.r;
    _sphere := tmp_select_record.s;
    dir_vec3 := tmp_select_record.dir;
    origin_vec3 := tmp_select_record.org;
    sphere_location_vec3 := tmp_select_record.loc;

    Dx := dir_vec3.x;
    Dy := dir_vec3.y;
    Dz := dir_vec3.z;
    Cx := sphere_location_vec3.x;
    Cy := sphere_location_vec3.y;
    Cz := sphere_location_vec3.z;
    Ox := origin_vec3.x;
    Oy := origin_vec3.y;
    Oz := origin_vec3.z;
    radius := _sphere.radius;
    quad_A := Dx ^ 2 + Dy ^ 2 + Dz ^ 2;
    quad_B := 2 * (Dx * (Ox - Cx) + Dy * (Oy - Cy) + Dz * (Oz - Cz));
    quad_C := (Ox - Cx) ^ 2 + (Oy - Cy) ^ 2 + (Oz - Cz) ^ 2 - (radius ^ 2);
    discriminant := (quad_B ^ 2) - (4 * quad_A * quad_C);

    if discriminant < 0 then
        return null;
    end if;

    t1 := (-quad_B - sqrt(discriminant)) / (2 * quad_A);
    t2 := (-quad_B + sqrt(discriminant)) / (2 * quad_A);

    t := null;
    if t1 > epsilon then
        t := t1;
    end if;
    if t2 > epsilon and (t is null or t2 < t) then
        t := t2;
    end if;
    if t is null then
        return null;
    end if;

    intersect_x := Ox + (Dx * t);
    intersect_y := Oy + (Dy * t);
    intersect_z := Oz + (Dz * t);

    insert into vec3 (x, y, z)
    values (intersect_x, intersect_y, intersect_z)
    returning id
        into intersection_vec3d_id;

    insert into intersection (ray_id, sphere_id, point_vec3_id, distance)
    values (ray_id, sphere_id, intersection_vec3d_id, t)
    returning id
        into intersection_id;

    return intersection_id;
end;
$$ language plpgsql;

create or replace function first_intersection(ray_id id_t) returns id_t as
$$
declare
    intersection_id id_t;
begin
    select id
    into intersection_id
    from (select ray_sphere(ray_id, s.id) as id
          from sphere s) _intersection
    where id is not null
    order by (select distance
              from intersection
              where id = _intersection.id) asc
    limit 1;
    return intersection_id;
end;
$$ language plpgsql;

create or replace function ray_direction(x integer, y integer, deflection double precision) returns id_t as
$$
declare
    width                  integer          := (select property_get('width'));
    height                 integer          := (select property_get('height'));
    deflect_x              double precision := -deflection + ((2 * deflection) / width) * x;
    deflect_y              double precision := -deflection + ((2 * deflection) / height) * y;
    dir_vec3_id            id_t;
    normalized_dir_vec3_id id_t;
begin
    insert into vec3 (x, y, z)
    values (deflect_x, deflect_y, 100)
    returning vec3.id
        into dir_vec3_id;
    normalized_dir_vec3_id := vec_normalize(dir_vec3_id);
    return normalized_dir_vec3_id;
end;
$$ language plpgsql;

create or replace function screen_ray(_x integer, _y integer, deflection double precision) returns id_t as
$$
declare
    width       integer = (select property_get('width'));
    height      integer = (select property_get('height'));
    ray_id      id_t;
    org_vec3_id id_t;
    dir_vec3_id id_t    = (select ray_direction(_x, _y, deflection));
begin
    insert into vec3 (x, y, z)
    values (_x, _y, 0)
    returning id
        into org_vec3_id;

    insert into ray (origin_vec3_id, direction_vec3_id)
    values (org_vec3_id, dir_vec3_id)
    returning id
        into ray_id;

    return ray_id;
end;
$$ language plpgsql;

create or replace function surface_normal(intersection_id id_t) returns id_t as
$$
declare
    tmp_select_record         record;
    _intersection             intersection%rowType;
    _sphere                   sphere%rowType;
    normal_vec3_id            id_t;
    normalized_normal_vec3_id id_t;
begin
    select i, s
    into tmp_select_record
    from intersection i
             join sphere s on s.id = i.sphere_id
    where i.id = intersection_id;
    _intersection := tmp_select_record.i;
    _sphere := tmp_select_record.s;

    normal_vec3_id := vec_sub(_intersection.point_vec3_id, _sphere.location_vec3_id);
    normalized_normal_vec3_id := vec_normalize(normal_vec3_id);

    return normalized_normal_vec3_id;
end;
$$ language plpgsql;

create or replace function sky_gradient(max_t double precision, t double precision,
                                        out r integer,
                                        out g integer,
                                        out b integer) returns record as
$$
declare
    from_r       integer := 0.5 * 255;
    from_g       integer := 0.7 * 255;
    from_b       integer := 255;
    to_r         integer := 255;
    to_g         integer := 255;
    to_b         integer := 255;
    fade         double precision;
    inverse_fade double precision;
begin
    fade := (1.0 / max_t) * t::double precision;
    inverse_fade := 1.0 - fade;
    select (from_r * inverse_fade + to_r * fade),
           (from_g * inverse_fade + to_g * fade),
           (from_b * inverse_fade + to_b * fade)
    into r, g, b;
end;
$$ language plpgsql;


--   ######################
--     Renderer Functions
--   ######################

create or replace function write_output_ppm() returns id_t as
$$
declare
    width            integer := (select property_get('width'));
    height           integer := (select property_get('height'));
    ppm_header       text    := format(E'P3\n%s %s\n255\n', width, height);
    ppm_body         text;
    render_output_id id_t;
begin
    raise info 'Writing output image as PPM';
    select string_agg(format('%s %s %s', p.r, p.g, p.b), E'\n' order by p.id)
    into ppm_body
    from pixel p;

    insert into render_output (rendered_at, ppm)
    values (now(), ppm_header || ppm_body)
    returning id
        into render_output_id;
    return render_output_id;
end;
$$ language plpgsql;

---------- Shading Functions ----------

create or replace function shade_diffuse(surf_normal_vec3_id id_t, sun_dir_vec3_id id_t,
                                         ambient_intensity double precision, sphere_id id_t) returns void as
$$
declare
    flipped_surf_normal_vec3_id id_t;
    diffuse_intensity           double precision;
    sphere_color                color%rowtype;
begin
    select c.*
    into sphere_color
    from sphere s
             join color c on c.id = s.color_id
    where s.id = sphere_id;

    flipped_surf_normal_vec3_id := vec_flip(surf_normal_vec3_id);

    diffuse_intensity := max(ambient_intensity, vec_dot(flipped_surf_normal_vec3_id, sun_dir_vec3_id));

    perform accumulator_sample((clamp_pixel_value(sphere_color.r::double precision * diffuse_intensity))::integer,
                               (clamp_pixel_value(sphere_color.g::double precision * diffuse_intensity))::integer,
                               (clamp_pixel_value(sphere_color.b::double precision * diffuse_intensity))::integer);
end;
$$ language plpgsql;

create or replace function shade_mirror(surf_normal_vec3_id id_t, sphere_id id_t,
                                        incident_direction_vec3_id id_t,
                                        intersection_point_vec3_id id_t) returns void as
$$
declare
    sphere_color        color%rowtype;
    reflected_vector_id id_t;
    reflected_ray_id    id_t;
begin
    select c.*
    into sphere_color
    from sphere s
             join color c on c.id = s.color_id
    where s.id = sphere_id;

    reflected_vector_id := vec_normalize(vec_reflect(incident_direction_vec3_id, surf_normal_vec3_id));
    reflected_ray_id := ray_new(intersection_point_vec3_id, reflected_vector_id);

    perform accumulator_sample(sphere_color.r, sphere_color.g, sphere_color.b);
    perform trace_ray(reflected_ray_id);
end;
$$ language plpgsql;

---------------------------------------

create or replace function trace_ray(ray_id id_t) returns void as
$$
declare
    MAX_BOUNCES constant           integer = 10;
    environment_id                 id_t    := property_environment_id();
    ray_direction_vec3_id          id_t;
    first_intersection_id          id_t;
    ambient_intensity              double precision;
    camera_dir_vec3_id             id_t;
    sun_dir_vec3_id                id_t;
    surf_normal_vec3_id            id_t;
    _sphere                        sphere%rowtype;
    incident_ray_direction_vec3_id id_t;
    incident_ray_elevation         double precision;
    intersection_point_vec3_id     id_t;
    tmp_color_record               record;
begin
    if bounces_count() >= MAX_BOUNCES then
        return;
    end if;

    perform bounces_increment();

    first_intersection_id := (select first_intersection(ray_id));

    select atan(v.y / sqrt(v.x ^ 2 + v.z ^ 2))
    into incident_ray_elevation
    from ray r
             join vec3 v on v.id = r.direction_vec3_id
    where r.id = ray_id;

    if first_intersection_id is null then
        select r, g, b from sky_gradient(1, (incident_ray_elevation + pi() / 2) / pi()) into tmp_color_record;
        perform accumulator_sample(tmp_color_record.r, tmp_color_record.g, tmp_color_record.b);
        return;
    end if;

    select r.direction_vec3_id, e.sun_direction_vec3_id, e.ambient_light_intensity, e.camera_direction_vec3_id
    into ray_direction_vec3_id, sun_dir_vec3_id, ambient_intensity, camera_dir_vec3_id
    from ray r
             join environment e on e.id = environment_id
    where r.id = ray_id;

    select s.*
    into _sphere
    from intersection i
             join sphere s on s.id = i.sphere_id
    where i.id = first_intersection_id;

    select r.direction_vec3_id, i.point_vec3_id
    into incident_ray_direction_vec3_id, intersection_point_vec3_id
    from intersection i
             join ray r on r.id = i.ray_id
    where i.id = first_intersection_id;

    surf_normal_vec3_id := surface_normal(first_intersection_id);

    if _sphere.shader = 'diffuse'::shader_type then
        perform shade_diffuse(surf_normal_vec3_id, sun_dir_vec3_id, ambient_intensity, _sphere.id);
    elsif _sphere.shader = 'mirror'::shader_type then
        perform shade_mirror(surf_normal_vec3_id, _sphere.id, incident_ray_direction_vec3_id,
                             intersection_point_vec3_id);
    end if;
end;
$$ language plpgsql;

create or replace function render() returns void as
$$
declare
    width                   integer          := property_width();
    height                  integer          := property_height();
    environment_id          id_t             := property_environment_id();
--
    plane_size              double precision;
    camera_loc_vec3_id      id_t;
    camera_dir_vec3_id      id_t;
--
    pixel_count             integer          := width * height;
    pixel_index             integer          := 0;
    tmp_coordinate_record   record;
    tmp_color_record        record;
--
    aspect_ratio            double precision := width::double precision / height::double precision;
    plane_height            double precision;
    plane_width             double precision;
--
    world_up_vec3_id        id_t;
    plane_right_vec3_id     id_t;
    plane_up_vec3_id        id_t;
    plane_top_left_vec3_id  id_t;
    plane_distance constant double precision := 1.0;
    plane_pixel_width       double precision;
    plane_pixel_height      double precision;
--
    tmp_vec3_id             id_t;
    tmp2_vec3_id            id_t;
--
    tmp_ray                 id_t;
--
    last_percentage         integer          := 0;
    current_percentage      integer          := 0;
--
    render_start_timestamp  timestamptz;
begin
    raise info 'Started rendering scene with % object(s) to an %x%px output image', (select count(*) from sphere), width, height;

    select e.view_plane_size, e.camera_direction_vec3_id, e.camera_location_vec3_id
    into plane_size, camera_dir_vec3_id, camera_loc_vec3_id
    from environment e
    where e.id = environment_id;

    plane_height := plane_size;
    plane_width := plane_size * aspect_ratio;

    plane_pixel_width := plane_width / width;
    plane_pixel_height := plane_height / height;

    world_up_vec3_id := vec_new(0, 1, 0);
    plane_right_vec3_id := vec_normalize(vec_cross(camera_dir_vec3_id, world_up_vec3_id));
    plane_up_vec3_id := vec_normalize(vec_cross(plane_right_vec3_id, camera_dir_vec3_id));

    -- Offset to the desired plane distance
    plane_top_left_vec3_id := vec_multiply(camera_dir_vec3_id, plane_distance);

    -- Move left 1/2 of the plane width
    tmp_vec3_id := vec_flip(plane_right_vec3_id);
    tmp_vec3_id := vec_multiply(tmp_vec3_id, plane_width / 2);
    plane_top_left_vec3_id := vec_add(plane_top_left_vec3_id, tmp_vec3_id);

    -- Move up 1/2 of the plane height
    tmp_vec3_id := vec_multiply(plane_up_vec3_id, plane_height / 2);
    plane_top_left_vec3_id := vec_add(plane_top_left_vec3_id, tmp_vec3_id);

    -- Shift plane to the camera location
    plane_top_left_vec3_id := vec_add(plane_top_left_vec3_id, camera_loc_vec3_id);

    render_start_timestamp := clock_timestamp();

    while
        pixel_index < pixel_count
        loop
            current_percentage =
                    (((pixel_index + 1)::double precision / pixel_count::double precision) *
                     100::double precision)::integer;

            -- Convert pixel index to cartesian coordinates
            select *
            from itoxy(pixel_index)
            into tmp_coordinate_record;

            -- Calculate pixel position on the image plane
            tmp_vec3_id := vec_copy(plane_right_vec3_id);
            tmp_vec3_id := vec_multiply(tmp_vec3_id, (tmp_coordinate_record.x + 0.5) * plane_pixel_width);

            tmp2_vec3_id := vec_flip(plane_up_vec3_id);
            tmp2_vec3_id := vec_multiply(tmp2_vec3_id, (tmp_coordinate_record.y + 0.5) * plane_pixel_height);

            tmp_vec3_id := vec_add(tmp_vec3_id, tmp2_vec3_id);
            tmp_vec3_id := vec_add(tmp_vec3_id, plane_top_left_vec3_id);

            -- Create ray
            tmp_ray := ray_new_plane(camera_loc_vec3_id, tmp_vec3_id);

            -- Shade and push the current pixel
            perform bounces_clear();
            select *
            from trace_ray(tmp_ray)
            into tmp_color_record;

            perform accumulator_commit();

            pixel_index := pixel_index + 1;

            if last_percentage + 4 < current_percentage then
                raise info 'Progress: % (Remaining: %)', (current_percentage || '%'), format_interval(
                        ((clock_timestamp() - render_start_timestamp) / pixel_index) *
                        (pixel_count - pixel_index));
                last_percentage := current_percentage;
            end if;

        end loop;
end;
$$ language plpgsql;

create or replace function setup_scene() returns void as
$$
declare
    _color_id id_t;
    vec3_id   id_t;
begin
    _color_id := color_new(231, 76, 60);
    vec3_id := vec_new(11, 0, 50);
    insert into sphere (color_id, location_vec3_id, radius, shader) values (_color_id, vec3_id, 10, 'mirror');

    _color_id := color_new(46, 204, 113);
    vec3_id := vec_new(-11, 0, 50);
    insert into sphere (color_id, location_vec3_id, radius, shader) values (_color_id, vec3_id, 10, 'diffuse');

    _color_id := color_new(255, 255, 255);
    vec3_id := vec_new(0, -1000, 30);
    insert into sphere (color_id, location_vec3_id, radius, shader) values (_color_id, vec3_id, 1000, 'mirror');
end;
$$ language plpgsql;

create or replace function reset_state() returns void as
$$
begin
    delete from intersection;
    delete from sphere;
    delete from color;
    delete from property;
    delete from pixel;
    delete from ray;
    delete from vec3;
    delete from environment;
end;
$$ language plpgsql;

create or replace function configure_render_params(width int default 300,
                                                   height int default 300,
                                                   sun_dir_x double precision default -1,
                                                   sun_dir_y double precision default -1,
                                                   sun_dir_z double precision default -1,
                                                   ambient_light double precision default 0,
                                                   _view_plane_size double precision default 1,
                                                   camera_loc_x double precision default 0,
                                                   camera_loc_y double precision default 0,
                                                   camera_loc_z double precision default 0,
                                                   camera_dir_x double precision default 0,
                                                   camera_dir_y double precision default 0,
                                                   camera_dir_z double precision default 1) returns void as
$$
declare
    sun_dir_vec3_id               id_t;
    sun_dir_normalized_vec3_id    id_t;
    camera_loc_vec3_id            id_t;
    camera_dir_vec3_id            id_t;
    camera_dir_normalized_vec3_id id_t;
    environment_id                id_t;
begin
    raise info 'Renderer set to %x%px output image. Sun direction is (%, %, %).', width, height, sun_dir_x, sun_dir_y, sun_dir_z;

    sun_dir_vec3_id := vec_new(sun_dir_x, sun_dir_y, sun_dir_z);
    sun_dir_normalized_vec3_id := vec_normalize(sun_dir_vec3_id);

    camera_loc_vec3_id := vec_new(camera_loc_x, camera_loc_y, camera_loc_z);

    camera_dir_vec3_id := vec_new(camera_dir_x, camera_dir_y, camera_dir_z);
    camera_dir_normalized_vec3_id := vec_normalize(camera_dir_vec3_id);

    insert into environment (sun_direction_vec3_id,
                             ambient_light_intensity,
                             camera_location_vec3_id,
                             camera_direction_vec3_id,
                             view_plane_size)
    values (sun_dir_normalized_vec3_id,
            ambient_light,
            camera_loc_vec3_id,
            camera_dir_normalized_vec3_id,
            _view_plane_size)
    returning id
        into environment_id;

    perform property_set_dimensions(width, height);
    perform property_set_environment(environment_id);
end;
$$ language plpgsql;

do
$$
    declare
        start_time timestamptz;
        end_time   timestamptz;
    begin
        start_time := clock_timestamp();
        perform reset_state();
        perform configure_render_params(200, 200, 1, -1, 1, 0.25);
        perform setup_scene();
        perform render();
        perform write_output_ppm();
        end_time := clock_timestamp();
        raise info 'Done (%)', format_interval_between(start_time, end_time);
    end;
$$ language plpgsql;