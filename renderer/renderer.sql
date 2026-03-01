-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The Unhinged PL/pgSQL Ray-Tracing Engine v2.0
--
-- (C) 2026 Piotr K. Wyrwwas, Michael T. Steinmötzger
-- This work is licensed under GPL-3.0
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

drop schema if exists pgray cascade;
create schema pgray;
set search_path to pgray;

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
    id                      bigint primary key generated always as identity not null,
    sun_direction_vec3_id   bigint                                          not null,
    ambient_light_intensity double precision                                not null,

    foreign key (sun_direction_vec3_id) references vec3 (id)
);

create unlogged table sphere
(
    id               bigint primary key generated always as identity not null,
    color_id         bigint                                          not null,
    location_vec3_id bigint                                          not null,
    radius           double precision                                not null,

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

--
-- ~~ Utility Functions ~~
--

create or replace function set_property(key text, value bigint) returns void as
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

create or replace function get_property(key text) returns text as
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

-- Converts an index to a cartesian (x, y) coordinate
create or replace function itoxy(i integer, x out integer, y out integer) returns record as
$$
declare
    width integer := (select get_property('width'));
begin
    select i / width, i % width into y, x;
end;
$$ language plpgsql;

-- Converts a (x, y) coordinate to a pixel index
create or replace function xytoi(x integer, y integer) returns integer as
$$
declare
    width integer := (select get_property('width'));
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

--
-- ~~ Math Functions ~~
--

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
    norm_id   id_t;
begin
    if magnitude = 0 then
        return vec3_id;
    end if;

    select id, x, y, z
    from vec3
    where id = vec3_id
    into vec_row;

    insert into vec3 (x, y, z)
    values (vec_row.x / magnitude, vec_row.y / magnitude, vec_row.z / magnitude)
    returning id into norm_id;
    return norm_id;
end;
$$ language plpgsql;

create or replace function vec_sub(a_vec3_id id_t, b_vec3_id id_t) returns id_t as
$$
declare
    vec_a  vec3%rowType;
    vec_b  vec3%rowType;
    out_id id_t;
begin
    select v.* from vec3 v where id = a_vec3_id into vec_a;
    select v.* from vec3 v where id = b_vec3_id into vec_b;
    insert into vec3 (x, y, z)
    values (vec_a.x - vec_b.x, vec_a.y - vec_b.y, vec_a.z - vec_b.z)
    returning id into out_id;
    return out_id;
end;
$$ language plpgsql;

create or replace function vec_flip(vec3_id id_t) returns id_t as
$$
declare
    _vec3           vec3%rowtype;
    flipped_vec3_id id_t;
begin
    select v.* from vec3 v where id = vec3_id into _vec3;
    insert into vec3 (x, y, z) values (-_vec3.x, -_vec3.y, -_vec3.z) returning id into flipped_vec3_id;
    return flipped_vec3_id;
end;
$$ language plpgsql;

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
    returning id into intersection_vec3d_id;

    insert into intersection (ray_id, sphere_id, point_vec3_id, distance)
    values (ray_id, sphere_id, intersection_vec3d_id, t)
    returning id into intersection_id;

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
    width                  integer          := (select get_property('width'));
    height                 integer          := (select get_property('height'));
    deflect_x              double precision := -deflection + ((2 * deflection) / width) * x;
    deflect_y              double precision := -deflection + ((2 * deflection) / height) * y;
    dir_vec3_id            id_t;
    normalized_dir_vec3_id id_t;
begin
    insert into vec3 (x, y, z) values (deflect_x, deflect_y, 100) returning vec3.id into dir_vec3_id;
    normalized_dir_vec3_id := vec_normalize(dir_vec3_id);
    return normalized_dir_vec3_id;
end;
$$ language plpgsql;

create or replace function screen_ray(_x integer, _y integer, deflection double precision) returns id_t as
$$
declare
    width       integer = (select get_property('width'));
    height      integer = (select get_property('height'));
    ray_id      id_t;
    org_vec3_id id_t;
    dir_vec3_id id_t    = (select ray_direction(_x, _y, deflection));
begin
    insert into vec3 (x, y, z) values (_x, _y, 0) returning id into org_vec3_id;

    insert into ray (origin_vec3_id, direction_vec3_id)
    values (org_vec3_id, dir_vec3_id)
    returning id into ray_id;

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

--
-- ~~ Rendering Functions ~~
--

create or replace function build_render_output() returns id_t as
$$
declare
    width            integer := (select get_property('width'));
    height           integer := (select get_property('height'));
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
    returning id into render_output_id;
    return render_output_id;
end;
$$ language plpgsql;

-- A simple gradient color pattern. For debugging purposes only.
create or replace function color_sweep(x integer, y integer, out r integer, out g integer, out b integer) returns record as
$$
declare
    width        integer := (select get_property('width'));
    height       integer := (select get_property('height'));
    from_r       integer := 0.5 * 255;
    from_g       integer := 0.7 * 255;
    from_b       integer := 255;
    to_r         integer := 255;
    to_g         integer := 255;
    to_b         integer := 255;
    fade         double precision;
    inverse_fade double precision;
begin
    fade := (1.0 / height::double precision) * y::double precision;
    inverse_fade := 1.0 - fade;
    select (from_r * inverse_fade + to_r * fade),
           (from_g * inverse_fade + to_g * fade),
           (from_b * inverse_fade + to_b * fade)
    into r, g, b;
end;
$$ language plpgsql;

create or replace function trace_ray(x integer, y integer, deflection double precision, out r integer, out g integer,
                                     out b integer) returns record as
$$
declare
    ray_id                      id_t := (select screen_ray(x, y, deflection));
    ray_direction_vec3_id       id_t;
    inter_id                    id_t := (select first_intersection(ray_id));
    environment_id              id_t := (select get_property('environment'));
    ambient_intensity           double precision;
    sun_dir_vec3_id             id_t;
    surf_normal_vec3_id         id_t;
    flipped_surf_normal_vec3_id id_t;
    flipped_surf_normal_vec3    vec3%rowType;
    diffuse_intensity           double precision;
    sphere_color                color%rowtype;
begin
    if inter_id is null then
        select * from color_sweep(x, y) into r, g, b;
        return;
    end if;

    select r.direction_vec3_id into ray_direction_vec3_id from ray r where r.id = ray_id;
    select e.sun_direction_vec3_id into sun_dir_vec3_id from environment e where e.id = environment_id;
    select e.ambient_light_intensity into ambient_intensity from environment e where e.id = environment_id;

    select c.*
    into sphere_color
    from intersection i
             join sphere s on i.sphere_id = s.id
             join color c on c.id = s.color_id
    where i.id = inter_id;

    surf_normal_vec3_id := surface_normal(inter_id);
    flipped_surf_normal_vec3_id := vec_flip(surf_normal_vec3_id);
    select v.* into flipped_surf_normal_vec3 from vec3 v where id = flipped_surf_normal_vec3_id;

    diffuse_intensity := max(ambient_intensity, vec_dot(flipped_surf_normal_vec3_id, sun_dir_vec3_id));

    r := (clamp_pixel_value(sphere_color.r::double precision * diffuse_intensity))::integer;
    g := (clamp_pixel_value(sphere_color.g::double precision * diffuse_intensity))::integer;
    b := (clamp_pixel_value(sphere_color.b::double precision * diffuse_intensity))::integer;
end;
$$ language plpgsql;

create or replace function render() returns void as
$$
declare
    width                 integer := (select get_property('width'));
    height                integer := (select get_property('height'));
    pixel_count           integer := width * height;
    pixel_index           integer := 0;
    tmp_coordinate_record record;
    tmp_color_record      record;
    last_percentage       integer = 0;
    current_percentage    integer = 0;
begin
    raise info 'Started rendering scene with % object(s) to an %x%px output image', (select count(*) from sphere), width, height;

    perform set_property('pixel_count', pixel_count);

    while pixel_index < pixel_count
        loop
            current_percentage =
                    (((pixel_index + 1)::double precision / pixel_count::double precision) *
                     100::double precision)::integer;

            if last_percentage + 4 < current_percentage then
                raise info 'Progress: %', current_percentage || '%';
                last_percentage := current_percentage;
            end if;

            -- Convert pixel index to cartesian coordinates
            select * from itoxy(pixel_index) into tmp_coordinate_record;

            -- Shade and push the current pixel
            select * from trace_ray(tmp_coordinate_record.x, tmp_coordinate_record.y, 0.001) into tmp_color_record;
            insert into pixel (r, g, b) values (tmp_color_record.r, tmp_color_record.g, tmp_color_record.b);

            pixel_index := pixel_index + 1;
        end loop;
end;
$$ language plpgsql;

create or replace function setup_scene() returns void as
$$
declare
    color_id id_t;
    vec3_id  id_t;
    width    integer := (select get_property('width'));
    height   integer := (select get_property('height'));
begin
    insert into color (r, g, b) values (255, 0, 0) returning id into color_id;
    insert into vec3 (x, y, z) values (width / 2, height / 2, 55) returning id into vec3_id;
    insert into sphere (color_id, location_vec3_id, radius) values (color_id, vec3_id, 25.0);

    insert into vec3 (x, y, z) values (width / 4, height / 4, 20) returning id into vec3_id;
    insert into sphere (color_id, location_vec3_id, radius) values (color_id, vec3_id, 5.0);
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
                                                   ambient_light double precision default 0) returns void as
$$
declare
    sun_dir_vec3_id            id_t;
    sun_dir_normalized_vec3_id id_t;
    environment_id             id_t;
begin
    raise info 'Renderer set to %x%px output image. Sun direction is (%, %, %).', width, height, sun_dir_x, sun_dir_y, sun_dir_z;

    insert into vec3 (x, y, z) values (sun_dir_x, sun_dir_y, sun_dir_z) returning id into sun_dir_vec3_id;
    sun_dir_normalized_vec3_id := vec_normalize(sun_dir_vec3_id);

    insert into environment (sun_direction_vec3_id, ambient_light_intensity)
    values (sun_dir_normalized_vec3_id, ambient_light)
    returning id into environment_id;

    perform set_property('width', width);
    perform set_property('height', height);
    perform set_property('environment', environment_id);
end;
$$ language plpgsql;

create or replace function format_interval(start_timestamp timestamptz, end_timestamp timestamptz) returns text as
$$
declare
    _interval interval := end_timestamp - start_timestamp;
    total_ms  int      := round(extract(epoch from _interval) * 1000);
    hours     int;
    minutes   int;
    seconds   int;
    millis    int;
    formatted text     := '';
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
do
$$
    declare
        start_time timestamptz;
        end_time   timestamptz;
    begin
        start_time := clock_timestamp();
        perform reset_state();
        perform configure_render_params(400, 400, -1, -1, 1, 0.25);
        perform setup_scene();
        perform render();
        perform build_render_output();
        end_time := clock_timestamp();
        raise info 'Done (%)', format_interval(start_time, end_time);
    end;
$$ language plpgsql;