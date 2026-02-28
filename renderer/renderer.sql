-- ~~~~~~~~~~~~~~~~~~~~~~
--   Piotr K. Wyrwwas
-- "The Mistake" #59328
-- ~~~~~~~~~~~~~~~~~~~~~~

drop schema if exists renderer cascade;
create schema renderer;

-- Used to store settings (since [global] variables are not a thing here)
create table map_entry
(
    key   text primary key,
    value integer
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

create table vector
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
    id       bigint primary key generated always as identity,
    color    bigint,
    location bigint,
    radius   double precision,

    foreign key (color) references color (id),
    foreign key (location) references vector (id)
);

create table ray
(
    id        bigint primary key generated always as identity,
    origin    bigint,
    direction bigint,

    foreign key (origin) references vector (id),
    foreign key (direction) references vector (id)
);

create table intersection
(
    id       bigint primary key generated always as identity,
    ray      bigint,
    sphere   bigint,
    point    bigint,
    distance double precision,

    foreign key (ray) references ray (id),
    foreign key (sphere) references sphere (id),
    foreign key (point) references vector (id)
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

-- Converts an index to a (x, y) coordinate
create or replace function itoxy(i integer, x out integer, y out integer) returns record as
$$
declare
    width  integer := (select get_var('width'));
    height integer := (select get_var('height'));
begin
    select i / width, i % height into y, x;
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

create or replace function dot(a bigint, b bigint) returns double precision as
$$
declare
    aVec vector%rowType;
    bVec vector%rowType;
begin
    select * from vector where id = a into aVec;
    select * from vector where id = b into bVec;
    return aVec.x * bVec.x + aVec.y * bVec.y + aVec.z * bVec.z;
end;
$$ language plpgsql;

create or replace function vec_magnitude(vec bigint) returns double precision as
$$
declare
    v vector%rowType;
begin
    select * from vector where id = vec into v;
    return sqrt(v.x ^ 2 + v.y ^ 2 + v.z ^ 2);
end;
$$ language plpgsql;

create or replace function vec_normalize(vec bigint) returns bigint as
$$
declare
    vec_row   vector%rowType;
    magnitude double precision = (select vec_magnitude(vec));
    norm_id   bigint;
begin
    if magnitude = 0 then
        return vec;
    end if;

    select id, x, y, z
    from vector
    where id = vec
    into vec_row;

    insert into vector (x, y, z)
    values (vec_row.x / magnitude, vec_row.y / magnitude, vec_row.z / magnitude)
    returning id into norm_id;
    return norm_id;
end;
$$ language plpgsql;

create or replace function vec_sub(a bigint, b bigint) returns bigint as
$$
declare
    vec_a  vector%rowType;
    vec_b  vector%rowType;
    out_id bigint;
begin
    select * from vector where id = a into vec_a;
    select * from vector where id = b into vec_b;
    insert into vector (x, y, z)
    values (vec_a.x - vec_b.x, vec_a.y - vec_b.y, vec_a.z - vec_b.z)
    returning id into out_id;
    return out_id;
end;
$$ language plpgsql;

create or replace function ray_sphere(ray_id bigint, sphere_id bigint) returns bigint as
$$
declare
    ray_row         ray%rowType;
    sphere_row      sphere%rowType;
    direction       vector%rowType;
    origin          vector%rowType;
    sphereLoc       vector%rowType;
    Dx              double precision;
    Dy              double precision;
    Dz              double precision;
    Cx              double precision;
    Cy              double precision;
    Cz              double precision;
    Ox              double precision;
    Oy              double precision;
    Oz              double precision;
    r               double precision;
    quad_A          double precision;
    quad_B          double precision;
    quad_C          double precision;
    discriminant    double precision;
    t1              double precision;
    t2              double precision;
    t               double precision;
    inter_x         double precision;
    inter_y         double precision;
    inter_z         double precision;
    render_distance double precision = (select get_var('render_distance'));
    point_uuid      bigint;
    inter_id        bigint;
begin
    select * from ray where id = ray_id into ray_row;
    select * from sphere where id = sphere_id into sphere_row;
    select * from vector where id = ray_row.direction into direction;
    select * from vector where id = ray_row.origin into origin;
    select * from vector where id = sphere_row.location into sphereLoc;

    Dx := direction.x;
    Dy := direction.y;
    Dz := direction.z;
    Cx := sphereLoc.x;
    Cy := sphereLoc.y;
    Cz := sphereLoc.z;
    Ox := origin.x;
    Oy := origin.y;
    Oz := origin.z;
    r := sphere_row.radius;
    quad_A := Dx ^ 2 + Dy ^ 2 + Dz ^ 2;
    quad_B := 2 * (Dx * (Ox - Cx) + Dy * (Oy - Cy) + Dz * (Oz - Cz));
    quad_C := (Ox - Cx) ^ 2 + (Oy - Cy) ^ 2 + (Oz - Cz) ^ 2 - r ^ 2;
    discriminant := quad_B ^ 2 - 4 * quad_A * quad_C;

    if discriminant < 0 then
        return null;
    end if;

    t1 := (-quad_B - sqrt(quad_B ^ 2 - 4 * quad_A * quad_B)) / 2 * quad_A;
    t2 := (-quad_B + sqrt(quad_B ^ 2 - 4 * quad_A * quad_B)) / 2 * quad_A;

    if t1 > 0.001 then
        t := t1;
    else
        if t2 > 0.001 then
            t := t2;
        else
            return null;
        end if;
    end if;

    inter_x := Ox + Dx * t;
    inter_y := Oy + Dy * t;
    inter_z := Oz + Dz * t;

    insert into vector (x, y, z) values (inter_x, inter_y, inter_z) returning id into point_uuid;
    insert into intersection (ray, sphere, point, distance)
    values (ray_id, sphere_id, point_uuid, t)
    returning id into inter_id;
    return inter_id;
end;
$$ language plpgsql;

create or replace function first_intersection(ray_id bigint) returns bigint as
$$
declare
    current_ray      ray%rowType;
    sphere           sphere%rowType       := null;
    sphere_count     integer              := (select count(*)
                                              from sphere);
    sphere_index     integer              := 0;
    first_inter      intersection%rowType = null;
    tmp_intersection intersection%rowType;
begin
    select * from ray where id = ray_id into current_ray;

    while sphere_index < sphere_count
        loop
            select * from sphere offset sphere_index limit 1 into sphere;
            select ray_sphere(ray_id, sphere.id) into tmp_intersection;
            if tmp_intersection is null then
                sphere_index := sphere_index + 1;
                continue;
            end if;
            if first_inter is null or tmp_intersection.distance < first_inter.distance then
                first_inter := tmp_intersection;
            end if;
            sphere_index := sphere_index + 1;
        end loop;

    return first_inter.id;
end;
$$ language plpgsql;

create or replace function ray_direction(x integer, y integer, deflection double precision) returns bigint as
$$
declare
    width     integer          := (select get_var('width'));
    height    integer          := (select get_var('height'));
    deflect_x double precision := -deflection + ((2 * deflection) / width) * x;
    deflect_y double precision := -deflection + ((2 * deflection) / height) * y;
--     deflect_x double precision := 0.0;
--     deflect_y double precision := 0.0;
    vec_uuid  bigint;
    norm_uuid bigint;
begin
    insert into vector (x, y, z) values (deflect_x, deflect_y, 100) returning vector.id into vec_uuid;
    select vec_normalize(vec_uuid) into norm_uuid;
    return norm_uuid;
end;
$$ language plpgsql;

create or replace function screen_ray(x integer, y integer, deflection double precision) returns bigint as
$$
declare
    width     integer = (select get_var('width'));
    height    integer = (select get_var('height'));
    origin    bigint;
    ray_id    bigint;
    direction bigint  = (select ray_direction(x, y, deflection));
begin
    insert into vector (x, y, z) values (x, y, 0) returning id into origin;
    insert into ray (origin, direction) values (origin, direction) returning id into ray_id;
    return ray_id;
end;
$$ language plpgsql;

create or replace function surface_normal(intersection_id bigint) returns bigint as
$$
declare
    inter_row   intersection%rowType;
    sphere_row  sphere%rowType;
    nor_id      bigint;
    norm_nor_id bigint;
begin
    select * from intersection where id = intersection_id into inter_row;
    select * from sphere where id = inter_row.sphere into sphere_row;
    select vec_sub(inter_row.point, sphere_row.location) into nor_id;
    select vec_normalize(nor_id) into norm_nor_id;
    return norm_nor_id;
end;
$$ language plpgsql;

--
-- ~~ Image Util Functions ~~
--

create or replace function build_ppm_str() returns text as
$$
declare
    width       integer       := (select get_var('width'));
    height      integer       := (select get_var('height'));
    pixel_count integer       := width * height;
    ppm         text          := format(E'P3\n%s\n%s\n255\n', width, height);
    pixelColor  pixel%rowType := null;
    index       integer       := 0;
begin
    select string_agg(format('%s %s %s', p.r, p.g, p.b), E'\n')
    into ppm
    from pixel p;
    --     while index < pixel_count
--         loop
--             select * from pixel limit 1 offset index into pixelColor;
--             ppm := concat(ppm, format(E'%s %s %s\n', pixelColor.r, pixelColor.g, pixelColor.b));
--             index := index + 1;
--         end loop;
    return format(E'P3\n%s %s\n255\n', width, height) || ppm;
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
    ray_id          bigint           = (select screen_ray(x, y, deflection));
    inter_id        bigint           = (select first_intersection(ray_id));
    inter_row       intersection%rowType;
    sphere_row      sphere%rowType;
    render_distance double precision = (select get_var('render_distance'));
    normal          bigint;
    normal_row      vector%rowType;
begin
    select * from intersection where id = inter_id into inter_row;
    if inter_id is not null then
        select * from sphere where id = inter_row.sphere into sphere_row;

        select surface_normal(inter_id) into normal;
        select * from vector where id = normal into normal_row;

        r := (255 / 2 + (normal_row.x * 255) / 2);
        g := (255 / 2 + (normal_row.y * 255) / 2);
        b := (255 / 2 + (normal_row.z * 255) / 2);
    else
        r := 0;
        g := 0;
        b := 0;
    end if;
end;
$$ language plpgsql;

create or replace function populate_pixels() returns void as
$$
declare
    width        integer := (select get_var('width'));
    height       integer := (select get_var('height'));
    pixel_count  integer := width * height;
    index        integer := 0;
    color_record record;
    coord_record record;
begin
    perform put_var('pixel_count', pixel_count);
    while index < pixel_count
        loop
        -- The color sweep requires X, Y coordinates; we don't have those.
        -- Thus, we need to convert the index to cartesian coords using our utility function
            select * from itoxy(index) into coord_record;

            -- Render a color palette; Just for debugging purposes.
            -- select * from color_sweep(coord_record.x, coord_record.y) into color_record;
            -- insert into pixel values (color_record.r, color_record.g, color_record.b);

            select * from trace_ray(coord_record.x, coord_record.y, 0.001) into color_record;
            insert into pixel values (color_record.r, color_record.g, color_record.b);
--                 insert into pixel values (255, 255, 255);
            index := index + 1;
        end loop;
end;
$$ language plpgsql;

create or replace function setup_scene() returns void as
$$
declare
    color_id bigint;
    vec_id   bigint;
    width    integer = (select get_var('width'));
    height   integer = (select get_var('height'));
begin
    insert into color (r, g, b) values (255, 0, 0) returning id into color_id;
    insert into vector (x, y, z) values (width / 2, height / 2, 50) returning id into vec_id;
    insert into sphere (color, location, radius) values (color_id, vec_id, 100.0);

    insert into vector (x, y, z) values (width / 4, height / 4, 20) returning id into vec_id;
    insert into sphere (color, location, radius) values (color_id, vec_id, 15.0);
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
    delete from vector;
end;
$$ language plpgsql;

create or replace function configure_renderer(width int default 300, height int default 300,
                                              render_distance int default 200) returns void as
$$
begin
    perform put_var('width', width);
    perform put_var('height', height);
    perform put_var('render_distance', render_distance);
end;
$$ language plpgsql;

select reset_state();
select configure_renderer(400, 400, 200);
select setup_scene();
select populate_pixels(); -- 2m 30s
select build_ppm_str();