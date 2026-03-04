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
    r  double precision                                not null,
    g  double precision                                not null,
    b  double precision                                not null
);

create unlogged table environment
(
    id                       bigint primary key generated always as identity not null,
    sky_intensity            double precision                                not null,
    camera_direction_vec3_id bigint                                          not null,
    camera_location_vec3_id  bigint                                          not null,
    view_plane_size          double precision                                not null,
    sample_count             integer                                         not null,
    bounce_limit             integer                                         not null,

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

create unlogged table bounces
(
    id bigint primary key generated always as identity not null
);

create unlogged table sample_accumulator
(
    id       bigint primary key generated always as identity not null,
    color_id bigint,

    foreign key (color_id) references color (id)
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


--   ###################
--     Color Functions
--   ###################

create or replace function color_new(_r double precision, _g double precision, _b double precision) returns id_t as
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

create or replace function color_new_int(_r integer, _g integer, _b integer) returns id_t as
$$
declare
    color_id id_t;
begin
    insert into color (r, g, b)
    values (_r::double precision / 255,
            _g::double precision / 255,
            _b::double precision / 255)
    returning id into color_id;
    return color_id;
end;
$$ language plpgsql;


create or replace function color_multiply_scalar(color_id id_t, factor double precision) returns id_t as
$$
declare
    _color       color%rowtype;
    new_color_id id_t;
begin
    select c.* into _color from color c where c.id = color_id;
    new_color_id := color_new(_color.r * factor, _color.g * factor, _color.b * factor);
    return new_color_id;
end;
$$ language plpgsql;

create or replace function color_multiply(a_color_id id_t, b_color_id id_t) returns id_t as
$$
declare
    a_color color%rowtype;
    b_color color%rowtype;
begin
    select c.* into a_color from color c where c.id = a_color_id;
    select c.* into b_color from color c where c.id = b_color_id;
    return color_new(a_color.r * b_color.r, a_color.g * b_color.g, a_color.b * b_color.b);
end;
$$ language plpgsql;

create or replace function color_add(a_color_id id_t, b_color_id id_t) returns id_t as
$$
declare
    a_color color%rowtype;
    b_color color%rowtype;
begin
    select c.* into a_color from color c where c.id = a_color_id;
    select c.* into b_color from color c where c.id = b_color_id;
    return color_new(a_color.r + b_color.r, a_color.g + b_color.g, a_color.b + b_color.b);
end;
$$ language plpgsql;


--   ################################
--     Sample Accumulator Functions
--   ################################

create or replace function accumulator_sample_new_int(_r int, _g int, _b int) returns void as
$$
begin
    insert into sample_accumulator (color_id) values (color_new_int(_r, _g, _b));
end;
$$ language plpgsql;

create or replace function accumulator_sample_new(_r double precision, _g double precision, _b double precision) returns void as
$$
begin
    insert into sample_accumulator (color_id) values (color_new(_r, _g, _b));
end;
$$ language plpgsql;

create or replace function accumulator_sample(_color_id id_t) returns void as
$$
begin
    insert into sample_accumulator (color_id) values (_color_id);
end;
$$ language plpgsql;

create or replace function accumulator_clear() returns void as
$$
begin
    delete from sample_accumulator;
end;
$$ language plpgsql;

create or replace function accumulator_commit(clear boolean default true) returns void as
$$
declare
    gamma constant double precision := 2.2;
--
    final_r        double precision := 0;
    final_g        double precision := 0;
    final_b        double precision := 0;
    color_vec3_id  id_t;
begin
    if (select count(*) from sample_accumulator) = 0 then
        insert into sample_accumulator (color_id) values (color_new(0, 0, 0));
    end if;

    select sum(c.r) / count(*), sum(c.g) / count(*), sum(c.b) / count(*)
    into final_r, final_g, final_b
    from sample_accumulator a
             join color c on c.id = a.color_id;

    color_vec3_id := vec_new(final_r ^ gamma, final_g ^ gamma, final_b ^ gamma);
    color_vec3_id := vec_normalize(color_vec3_id);
    color_vec3_id := vec_multiply(color_vec3_id, 255);

    select x::integer, y::integer, z::integer into final_r, final_g, final_b from vec3 v where v.id = color_vec3_id;

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

create or replace function vec_new_random_unit() returns id_t as
$$
declare
    _x double precision := random() * 2 - 1;
    _y double precision := random() * 2 - 1;
    _z double precision := random() * 2 - 1;
begin
    return vec_normalize(vec_new(_x, _y, _z));
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

create or replace function ray_sphere(_ray_id id_t, _sphere_id id_t) returns id_t as
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
             join sphere s on s.id = _sphere_id
             join vec3 dir on dir.id = r.direction_vec3_id
             join vec3 org on org.id = r.origin_vec3_id
             join vec3 loc on loc.id = s.location_vec3_id
    where r.id = _ray_id;

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

    if (t2 > epsilon) and (t is null or t2 < t) then
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
    values (_ray_id, _sphere_id, intersection_vec3d_id, t)
    returning id
        into intersection_id;

    return intersection_id;
end;
$$ language plpgsql;

create or replace function first_intersection(_ray_id id_t) returns id_t as
$$
declare
    _sphere             sphere%rowType       := null;
    sphere_count        integer              := (select count(*)
                                                 from sphere);
    sphere_index        integer              := 0;
    first_inter         intersection%rowType = null;
    tmp_intersection_id id_t                 = null;
    tmp_intersection    intersection%rowtype = null;
begin
    while sphere_index < sphere_count
        loop
            select s.* into _sphere from sphere s offset sphere_index limit 1;

            select ray_sphere(_ray_id, _sphere.id) into tmp_intersection_id;

            if tmp_intersection_id is null then
                sphere_index := sphere_index + 1;
                continue;
            end if;

            select i.* into tmp_intersection from intersection i where i.id = tmp_intersection_id;

            if first_inter is null or tmp_intersection.distance < first_inter.distance then
                first_inter := tmp_intersection;
            end if;

            sphere_index := sphere_index + 1;
        end loop;

    return first_inter.id;
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

create or replace function sky(direction_vec3_id id_t, sky_intensity double precision default 0) returns id_t as
$$
declare
--  255, 200, 150
    from_r                       integer := 255;
    from_g                       integer := 200;
    from_b                       integer := 150;
--  70, 130, 220
    to_r                         integer := 70;
    to_g                         integer := 130;
    to_b                         integer := 220;
--
    t                            double precision;
    fade                         double precision;
    inv_fade                     double precision;
    color_id                     id_t;
    incident_direction_elevation double precision;
begin
    select atan(v.y / sqrt(v.x ^ 2 + v.z ^ 2))
    into incident_direction_elevation
    from vec3 v
    where v.id = direction_vec3_id;

    t := (incident_direction_elevation + pi() / 2) / pi() - 0.25;

    fade := min(max(t::double precision, 0), 1);
    inv_fade := 1 - fade;

    color_id := color_new_int((((from_r * inv_fade + to_r * fade))::int),
                              (((from_g * inv_fade + to_g * fade))::int),
                              (((from_b * inv_fade + to_b * fade)))::int);

    color_id := color_multiply_scalar(color_id, sky_intensity);

    return color_id;
end;
$$ language plpgsql;


--   ######################
--     Renderer Functions
--   ######################

create or replace function write_output_ppm(width integer default null,
                                            height integer default null) returns id_t as
$$
declare
    img_width        integer := width;
    img_height       integer := height;
    ppm_header       text;
    ppm_body         text    = '';
    render_output_id id_t;
begin
    raise info 'Writing output image as PPM';

    if width is null then
        width := (select property_get('width'));
    end if;

    if height is null then
        height := (select property_get('height'));
    end if;

    ppm_header := format(E'P3\n%s %s\n255\n', width, height);

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

create or replace function shade_diffuse(surf_normal_vec3_id id_t, sphere_id id_t,
                                         intersection_point_vec3_id id_t) returns id_t as
$$
declare
    sphere_color_id            id_t;
    diffuse_reflection_vec3_id id_t;
    reflection_ray_id          id_t;
    diffuse_color_id           id_t;
begin
    select s.color_id into sphere_color_id from sphere s where s.id = sphere_id;

    diffuse_reflection_vec3_id := vec_new_random_unit();

    if vec_dot(diffuse_reflection_vec3_id, surf_normal_vec3_id) < 0 then
        diffuse_reflection_vec3_id := vec_multiply(diffuse_reflection_vec3_id, -1);
    end if;

    reflection_ray_id := ray_new(intersection_point_vec3_id, diffuse_reflection_vec3_id);
    diffuse_color_id := trace_ray(reflection_ray_id);
    diffuse_color_id := color_multiply(diffuse_color_id, sphere_color_id);
    diffuse_color_id := color_multiply_scalar(diffuse_color_id, 0.9);

    return diffuse_color_id;
end;
$$ language plpgsql;

create or replace function shade_mirror(surf_normal_vec3_id id_t, sphere_id id_t,
                                        incident_direction_vec3_id id_t,
                                        intersection_point_vec3_id id_t) returns id_t as
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

    return trace_ray(reflected_ray_id);
end;
$$ language plpgsql;

---------------------------------------

create or replace function trace_ray(ray_id id_t) returns id_t as
$$
declare
    environment_id                 id_t := property_environment_id();
    ray_direction_vec3_id          id_t;
    first_intersection_id          id_t;
    _sky_intensity                 double precision;
    camera_dir_vec3_id             id_t;
    surf_normal_vec3_id            id_t;
    _bounce_limit                  integer;
    _sphere                        sphere%rowtype;
    incident_ray_direction_vec3_id id_t;
    intersection_point_vec3_id     id_t;
begin
    first_intersection_id := (select first_intersection(ray_id));

    select r.direction_vec3_id,
           e.sky_intensity,
           e.camera_direction_vec3_id,
           e.bounce_limit
    into ray_direction_vec3_id, _sky_intensity, camera_dir_vec3_id, _bounce_limit
    from ray r
             join environment e on e.id = environment_id
    where r.id = ray_id;

    select r.direction_vec3_id into incident_ray_direction_vec3_id from ray r where r.id = ray_id;

    if first_intersection_id is null then
        return sky(incident_ray_direction_vec3_id, _sky_intensity);
    end if;

    select i.point_vec3_id into intersection_point_vec3_id from intersection i where i.id = first_intersection_id;

    if bounces_count() >= _bounce_limit then
        return color_new(0, 0, 0);
    end if;
    perform bounces_increment();

    select s.*
    into _sphere
    from intersection i
             join sphere s on s.id = i.sphere_id
    where i.id = first_intersection_id;

    surf_normal_vec3_id := surface_normal(first_intersection_id);

    if _sphere.shader = 'diffuse'::shader_type then
        return shade_diffuse(surf_normal_vec3_id, _sphere.id, intersection_point_vec3_id);
    elsif _sphere.shader = 'mirror'::shader_type then
        return shade_mirror(surf_normal_vec3_id, _sphere.id, incident_ray_direction_vec3_id,
                            intersection_point_vec3_id);
    end if;
end;
$$ language plpgsql;

create or replace function render(tile_x_from integer,
                                  tile_y_from integer,
                                  tile_x_to integer,
                                  tile_y_to integer) returns void as
$$
declare
    full_image_width        integer          := property_width();
    full_image_height       integer          := property_height();
    environment_id          id_t             := property_environment_id();
--
    plane_size              double precision;
    camera_loc_vec3_id      id_t;
    camera_dir_vec3_id      id_t;
    _sample_count           integer;
--
    tmp_color_id            id_t;
--
    aspect_ratio            double precision;
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
--
    pixel_x                 integer;
    pixel_y                 integer;
    tile_width              integer          := (tile_x_to - tile_x_from);
    tile_height             integer          := (tile_y_to - tile_y_from);
    tile_pixel_count        integer          := tile_width * tile_height;
    rendered_pixel_count    integer          := 0;
begin
    raise info 'Started rendering region (%, %; %, %) size (%, %)  of scene with % object(s). Original image size is %x%px', tile_x_from, tile_y_from, tile_x_to, tile_y_to, tile_width, tile_height, (select count(*) from sphere), full_image_width, full_image_height;

    if full_image_width = 0 or full_image_height = 0 or tile_width = 0 or tile_height = 0 then
        raise notice 'Width or height of tile or image are 0. Nothing to do.';
        return;
    end if;

    aspect_ratio := full_image_width::double precision / full_image_height::double precision;

    select e.view_plane_size, e.camera_direction_vec3_id, e.camera_location_vec3_id, e.sample_count
    into plane_size, camera_dir_vec3_id, camera_loc_vec3_id, _sample_count
    from environment e
    where e.id = environment_id;

    plane_height := plane_size;
    plane_width := plane_size * aspect_ratio;

    plane_pixel_width := plane_width / full_image_width;
    plane_pixel_height := plane_height / full_image_height;

    world_up_vec3_id := vec_new(0, 1, 0);
    plane_right_vec3_id := vec_normalize(vec_cross(world_up_vec3_id, camera_dir_vec3_id));
    plane_up_vec3_id := vec_normalize(vec_cross(camera_dir_vec3_id, plane_right_vec3_id));

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

    for pixel_y in tile_y_from .. tile_y_to - 1
        loop

            for pixel_x in tile_x_from .. tile_x_to - 1
                loop
                    current_percentage =
                            ((rendered_pixel_count::double precision / tile_pixel_count::double precision) *
                             100::double precision)::integer;

                    -- Calculate pixel position on the image plane
                    tmp_vec3_id := vec_copy(plane_right_vec3_id);
                    tmp_vec3_id := vec_multiply(tmp_vec3_id, (pixel_x + 0.5) * plane_pixel_width);

                    tmp2_vec3_id := vec_flip(plane_up_vec3_id);
                    tmp2_vec3_id := vec_multiply(tmp2_vec3_id, (pixel_y + 0.5) * plane_pixel_height);

                    tmp_vec3_id := vec_add(tmp_vec3_id, tmp2_vec3_id);
                    tmp_vec3_id := vec_add(tmp_vec3_id, plane_top_left_vec3_id);

                    -- Create ray
                    tmp_ray := ray_new_plane(camera_loc_vec3_id, tmp_vec3_id);

                    -- Collect samples
                    for _ in 1.._sample_count
                        loop
                            perform bounces_clear();

                            select *
                            from trace_ray(tmp_ray)
                            into tmp_color_id;

                            perform accumulator_sample(tmp_color_id);
                        end loop;

                    -- Commit the final sample sum
                    perform accumulator_commit();

                    rendered_pixel_count := rendered_pixel_count + 1;

                    if last_percentage + 4 < current_percentage then
                        raise info 'Progress: % (Remaining: %)', (current_percentage || '%'), format_interval(
                                ((clock_timestamp() - render_start_timestamp) / rendered_pixel_count) *
                                (tile_pixel_count - rendered_pixel_count));
                        last_percentage := current_percentage;
                    end if;

                end loop;

        end loop;
end;
$$ language plpgsql;

create or replace function setup_scene() returns void as
$$
declare
    _color_id                     id_t;
    vec3_id                       id_t;
--
    GROUND_SPHERE_RADIUS constant double precision := 100000;
begin
    --     _color_id := color_new_int(231, 76, 60);
--     vec3_id := vec_new(11, 0, 50);
--     insert into sphere (color_id, location_vec3_id, radius, shader) values (_color_id, vec3_id, 10, 'diffuse');

    _color_id := color_new_int(52, 152, 219);
    vec3_id := vec_new(0, 10, 50);
    insert into sphere (color_id, location_vec3_id, radius, shader)
    values (_color_id, vec3_id, 10, 'diffuse');

    _color_id := color_new_int(255, 255, 255);
    vec3_id := vec_new(0, -GROUND_SPHERE_RADIUS - 1, 50);
    insert into sphere (color_id, location_vec3_id, radius, shader)
    values (_color_id, vec3_id, GROUND_SPHERE_RADIUS, 'diffuse');
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
                                                   _sample_count integer default 1,
                                                   _bounce_limit integer default 10,
                                                   _sky_intensity double precision default 1,
                                                   _view_plane_size double precision default 2,
                                                   camera_loc_x double precision default 0,
                                                   camera_loc_y double precision default 10,
                                                   camera_loc_z double precision default 0,
                                                   camera_dir_x double precision default 0,
                                                   camera_dir_y double precision default -0.15,
                                                   camera_dir_z double precision default 1) returns void as
$$
declare
    camera_loc_vec3_id            id_t;
    camera_dir_vec3_id            id_t;
    camera_dir_normalized_vec3_id id_t;
    environment_id                id_t;
begin
    raise info 'Renderer set to %x%px output image.', width, height;

    camera_loc_vec3_id := vec_new(camera_loc_x, camera_loc_y, camera_loc_z);

    camera_dir_vec3_id := vec_new(camera_dir_x, camera_dir_y, camera_dir_z);
    camera_dir_normalized_vec3_id := vec_normalize(camera_dir_vec3_id);

    insert into environment (camera_location_vec3_id,
                             camera_direction_vec3_id,
                             view_plane_size,
                             sample_count,
                             bounce_limit,
                             sky_intensity)
    values (camera_loc_vec3_id,
            camera_dir_normalized_vec3_id,
            _view_plane_size,
            _sample_count,
            _bounce_limit,
            _sky_intensity)
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
        perform configure_render_params(100, 100, 50, 20, 1.0);
        perform setup_scene();
        perform render(0, 0, 100, 100);
        perform write_output_ppm(100, 100);
        end_time := clock_timestamp();
        raise info 'Done (%)', format_interval_between(start_time, end_time);
    end;
$$ language plpgsql;