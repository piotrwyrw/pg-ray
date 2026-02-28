-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The Unhinged PL/pgSQL Ray-Tracing Engine v2.0
--
-- (C) 2026 Piotr K. Wyrwwas, Michael T. Steinmötzger
-- This work is licensed under GPL-3.0
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

drop schema if exists pgray cascade;
create schema pgray;
set search_path to pgray;

create domain id_type as bigint;

-- Used to store settings (since global variables are not a thing here)
create table map_entry
(
    key   text primary key,
    value integer
);

create table render_output
(
    id          bigint primary key generated always as identity,
    rendered_at timestamptz,
    ppm         text
);

create table pixel
(
    r integer,
    g integer,
    b integer,

    constraint r check (r >= 0 and r <= 255),
    constraint g check (g >= 0 and g <= 255),
    constraint b check (b >= 0 and b <= 255)
);

create table vec3
(
    id bigint primary key generated always as identity,
    x  double precision,
    y  double precision,
    z  double precision
);

create table color
(
    id bigint primary key generated always as identity,
    r  integer,
    g  integer,
    b  integer
);

create table sphere
(
    id               bigint primary key generated always as identity,
    color_id         bigint,
    location_vec3_id bigint,
    radius           double precision,

    foreign key (color_id) references color (id),
    foreign key (location_vec3_id) references vec3 (id)
);

create table ray
(
    id                bigint primary key generated always as identity,
    origin_vec3_id    bigint,
    direction_vec3_id bigint,

    foreign key (origin_vec3_id) references vec3 (id),
    foreign key (direction_vec3_id) references vec3 (id)
);

create table intersection
(
    id            bigint primary key generated always as identity,
    ray_id        bigint,
    sphere_id     bigint,
    point_vec3_id bigint,
    distance      double precision,

    foreign key (ray_id) references ray (id),
    foreign key (sphere_id) references sphere (id),
    foreign key (point_vec3_id) references vec3 (id)
);

--
-- ~~ Utility Functions ~~
--

create or replace function put_var(key text, value integer) returns void as
$$
declare
    mapKey   constant text    := key;
    mapValue constant integer := value;
begin
    if exists (select * from map_entry where map_entry.key = mapKey) then
        update map_entry set value = mapValue where key = mapKey;
        return;
    end if;

    insert into map_entry values (key, value);
end;
$$ language plpgsql;

create or replace function get_var(key text) returns text as
$$
declare
    mapKey constant text    := key;
    resultCache     integer := null;
begin
    select map_entry.value as mapKey from map_entry where map_entry.key = mapKey into resultCache;
    if resultCache is null then
        raise 'Attempting to access undefined variable "%"!', key;
    end if;
    return resultCache;
end;
$$ language plpgsql;

-- Converts an index to a cartesian (x, y) coordinate
create or replace function itoxy(i integer, x out integer, y out integer) returns record as
$$
declare
    width integer := (select get_var('width'));
begin
    select i / width, i % width into y, x;
end;
$$ language plpgsql;

-- Converts a (x, y) coord to an index
create or replace function xytoi(x integer, y integer) returns integer as
$$
declare
    width integer := (select get_var('width'));
begin
    return y * width + x;
end ;
$$ language plpgsql;


--
-- ~~ Math Functions ~~
--

create or replace function dot(a_vec3_id id_type, b_vec3_id id_type) returns double precision as
$$
declare
    aVec vec3%rowType;
    bVec vec3%rowType;
begin
    select * from vec3 where id = a_vec3_id into aVec;
    select * from vec3 where id = b_vec3_id into bVec;
    return aVec.x * bVec.x + aVec.y * bVec.y + aVec.z * bVec.z;
end;
$$ language plpgsql;

create or replace function vec_magnitude(vec3_id id_type) returns double precision as
$$
declare
    v vec3%rowType;
begin
    select * from vec3 where id = vec3_id into v;
    return sqrt(v.x ^ 2 + v.y ^ 2 + v.z ^ 2);
end;
$$ language plpgsql;

create or replace function vec_normalize(vec3_id id_type) returns id_type as
$$
declare
    vec_row   vec3%rowType;
    magnitude double precision = (select vec_magnitude(vec3_id));
    norm_id   id_type;
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

create or replace function vec_sub(a_vec3_id id_type, b_vec3_id id_type) returns id_type as
$$
declare
    vec_a  vec3%rowType;
    vec_b  vec3%rowType;
    out_id id_type;
begin
    select * from vec3 where id = a_vec3_id into vec_a;
    select * from vec3 where id = b_vec3_id into vec_b;
    insert into vec3 (x, y, z)
    values (vec_a.x - vec_b.x, vec_a.y - vec_b.y, vec_a.z - vec_b.z)
    returning id into out_id;
    return out_id;
end;
$$ language plpgsql;

create or replace function ray_sphere(ray_id id_type, sphere_id id_type) returns id_type as
$$
declare
    epsilon               double precision := 10e-6;
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
    intersection_vec3d_id id_type;
    intersection_id       id_type;
begin
    select * from ray where id = ray_id into _ray;
    select * from sphere where id = sphere_id into _sphere;
    select * from vec3 where id = _ray.direction_vec3_id into dir_vec3;
    select * from vec3 where id = _ray.origin_vec3_id into origin_vec3;
    select * from vec3 where id = _sphere.location_vec3_id into sphere_location_vec3;

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

create or replace function first_intersection(ray_id id_type) returns id_type as
$$
declare
    intersection_id id_type;
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

create or replace function ray_direction(x integer, y integer, deflection double precision) returns id_type as
$$
declare
    width                  integer          := (select get_var('width'));
    height                 integer          := (select get_var('height'));
    deflect_x              double precision := -deflection + ((2 * deflection) / width) * x;
    deflect_y              double precision := -deflection + ((2 * deflection) / height) * y;
    dir_vec3_id            id_type;
    normalized_dir_vec3_id id_type;
begin
    insert into vec3 (x, y, z) values (deflect_x, deflect_y, 100) returning vec3.id into dir_vec3_id;
    select vec_normalize(dir_vec3_id) into normalized_dir_vec3_id;
    return normalized_dir_vec3_id;
end;
$$ language plpgsql;

create or replace function screen_ray(x integer, y integer, deflection double precision) returns id_type as
$$
declare
    width             integer = (select get_var('width'));
    height            integer = (select get_var('height'));
    origin_vec3_id    id_type;
    ray_id            id_type;
    direction_vec3_id id_type = (select ray_direction(x, y, deflection));
begin
    insert into vec3 (x, y, z) values (x, y, 0) returning id into origin_vec3_id;

    insert into ray (origin_vec3_id, direction_vec3_id)
    values (origin_vec3_id, direction_vec3_id)
    returning id into ray_id;

    return ray_id;
end;
$$ language plpgsql;

create or replace function surface_normal(intersection_id id_type) returns id_type as
$$
declare
    _intersection             intersection%rowType;
    _sphere                   sphere%rowType;
    normal_vec3_id            id_type;
    normalized_normal_vec3_id id_type;
begin
    select * from intersection where id = intersection_id into _intersection;
    select * from sphere where id = _intersection.sphere_id into _sphere;
    select vec_sub(_intersection.point_vec3_id, _sphere.location_vec3_id) into normal_vec3_id;
    select vec_normalize(normal_vec3_id) into normalized_normal_vec3_id;
    return normalized_normal_vec3_id;
end;
$$ language plpgsql;

--
-- ~~ Image Util Functions ~~
--

create or replace function build_render_output() returns id_type as
$$
declare
    width            integer := (select get_var('width'));
    height           integer := (select get_var('height'));
    ppm_header       text    := format(E'P3\n%s %s\n255\n', width, height);
    ppm_body         text;
    render_output_id id_type;
begin
    select string_agg(format('%s %s %s', p.r, p.g, p.b), E'\n')
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
    width  integer := (select get_var('width'));
    height integer := (select get_var('height'));
begin
    select (255 / width) * x, (255 / height) * y, (255 / height) * (height - y) into r, g, b;
end;
$$ language plpgsql;

create or replace function trace_ray(x integer, y integer, deflection double precision, out r integer, out g integer,
                                     out b integer) returns record as
$$
declare
    ray_id              id_type = (select screen_ray(x, y, deflection));
    inter_id            id_type = (select first_intersection(ray_id));
    _intersection       intersection%rowType;
    _sphere             sphere%rowType;
    surf_normal_vec3_id id_type;
    surf_normal_vec3    vec3%rowType;
begin
    if inter_id is null then
        r := 20;
        g := 20;
        b := 20;
        return;
    end if;

    select * from intersection where id = inter_id into _intersection;

    select * from sphere where id = _intersection.sphere_id into _sphere;

    select surface_normal(inter_id) into surf_normal_vec3_id;
    select * from vec3 where id = surf_normal_vec3_id into surf_normal_vec3;

    r := (255 / 2 + (surf_normal_vec3.x * 255) / 2);
    g := (255 / 2 + (surf_normal_vec3.y * 255) / 2);
    b := (255 / 2 + (surf_normal_vec3.z * 255) / 2);
end;
$$ language plpgsql;

create or replace function populate_pixels() returns void as
$$
declare
    width        integer := (select get_var('width'));
    height       integer := (select get_var('height'));
    pixel_count  integer := width * height;
    pixel_index  integer := 0;
    color_record record;
    coord_record record;
begin
    perform put_var('pixel_count', pixel_count);

    while pixel_index < pixel_count
        loop
        -- The color sweep requires X, Y coordinates; we don't have those.
        -- Thus, we need to convert the index to cartesian coords using our utility function
            select * from itoxy(pixel_index) into coord_record;

            select * from trace_ray(coord_record.x, coord_record.y, 0.001) into color_record;
            insert into pixel values (color_record.r, color_record.g, color_record.b);
--             insert into pixel values (255, 255, 255);
            pixel_index := pixel_index + 1;
        end loop;
end;
$$ language plpgsql;

create or replace function setup_scene() returns void as
$$
declare
    color_id id_type;
    vec3_id  id_type;
    width    integer = (select get_var('width'));
    height   integer = (select get_var('height'));
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
    delete from map_entry;
    delete from pixel;
    delete from ray;
    delete from vec3;
end;
$$ language plpgsql;

create or replace function configure_renderer(width int default 300, height int default 300) returns void as
$$
begin
    perform put_var('width', width);
    perform put_var('height', height);
end;
$$ language plpgsql;

select reset_state();
select configure_renderer(500, 500);
select setup_scene();
select populate_pixels();
select build_render_output();