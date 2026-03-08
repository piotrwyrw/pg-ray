/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

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