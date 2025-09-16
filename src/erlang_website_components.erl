-module(erlang_website_components).
-compile({parse_transform, arizona_parse_transform}).
-compile({no_auto_import,[link/1]}).
-export([link/1]).
-export([link_button/1]).
-export([button/1]).
-export([icon/1]).
-export([card/1]).

link(Bindings) ->
    arizona_template:from_string(~"""
    <a
        href="{arizona_template:get_binding(href, Bindings)}"
        class="{[
            ~"inline-flex items-center justify-center transition-smooth ",
            arizona_template:get_binding(extra_classes, Bindings, ~"")
        ]}"
    >
        {arizona_template:get_binding(label, Bindings, ~"")}
        {case arizona_template:find_binding(icon, Bindings) of
            {ok, Icon} ->
                arizona_template:render_stateless(erlang_website_components, icon, #{icon => Icon});
            error ->
                ~""
        end}
    </a>
    """).

link_button(Bindings) ->
    arizona_template:from_string(~"""
    <a
        href="{arizona_template:get_binding(href, Bindings)}"
        class="{[
            button_classes(arizona_template:get_binding(variant, Bindings, primary)),
            ~" ",
            arizona_template:get_binding(extra_classes, Bindings, ~"inline-flex")
        ]}"
    >
        {arizona_template:get_binding(label, Bindings)}
        {case arizona_template:find_binding(icon, Bindings) of
            {ok, Icon} ->
                arizona_template:render_stateless(erlang_website_components, icon, #{icon => Icon});
            error ->
                ~""
        end}
    </a>
    """).

button(Bindings) ->
    arizona_template:from_string(~"""
    <button
        id="{arizona_template:get_binding(id, Bindings)}"
        type="{arizona_template:get_binding(type, Bindings, ~"button")}"
        class="{[
            button_classes(arizona_template:get_binding(variant, Bindings, primary)),
            ~" ",
            arizona_template:get_binding(extra_classes, Bindings, ~"inline-flex")
        ]}"
    >
        {arizona_template:get_binding(label, Bindings, ~"")}
        {case arizona_template:find_binding(icon, Bindings) of
            {ok, Icon} ->
                arizona_template:render_stateless(erlang_website_components, icon, #{
                    icon => Icon
                });
            error ->
                ~""
        end}
    </button>
    """).

icon(Bindings) ->
    arizona_template:from_string(~"""
    <svg class="ml-2 w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="{icon_path(arizona_template:get_binding(icon, Bindings))}"
        ></path>
    </svg>
    """).

card(Bindings) ->
    Tag = arizona_binder:get(tag, Bindings, ~"div"),
    arizona_template:from_string(~"""
    <{Tag} class="{[
        ~"bg-gray-900 rounded-2xl p-8 border border-gray-700 ",
        ~"hover:border-gray-600 transition-smooth"
    ]}">
        {arizona_template:render_slot(arizona_template:get_binding(content, Bindings))}
    </{Tag}>
    """).

% Internal functions

button_classes(Variant) ->
    [
        ~"items-center justify-center transition-smooth ",
        ~"px-8 py-4 font-semibold rounded-lg cursor-pointer ",
        button_variant_classes(Variant)
    ].

button_variant_classes(primary) ->
    ~"text-white hover:shadow-lg [&:hover]:bg-erlang-red-dark [&:not(:hover)]:bg-erlang-red";
button_variant_classes(secondary) ->
    [
        ~"text-gray-300 hover:text-white hover:bg-gray-700 ",
        ~"border border-transparent hover:border-gray-600"
    ];
button_variant_classes(outline) ->
    ~"border-2 border-gray-600 hover:border-erlang-red text-gray-300 hover:text-erlang-red";
button_variant_classes(carousel_arrow) ->
    [
        ~"carousel-arrow absolute top-1/2 -translate-y-1/2 z-10 w-10 h-10 bg-gray-800 ",
        ~"hover:bg-erlang-red border-2 border-transparent hover:border-erlang-red-light ",
        ~"rounded-full text-white"
    ].

icon_path(download) ->
    ~"M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4 4m0 0l-4-4m4 4V4";
icon_path(bars) ->
    ~"M4 6h16M4 12h16M4 18h16";
icon_path(github) ->
    [
        ~"M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 ",
        ~"5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 ",
        ~"5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 ",
        ~"3.37 0 0 0 9 18.13V22"
    ];
icon_path(twitter) ->
    [
        ~"M23.953 4.57a10 10 0 01-2.825.775 4.958 4.958 0 002.163-2.723c-.951.555-2.005.959-3.127 ",
        ~"1.184a4.92 4.92 0 00-8.384 4.482C7.69 8.095 4.067 6.13 1.64 3.162a4.822 4.822 0 00-.666 ",
        ~"2.475c0 1.71.87 3.213 2.188 4.096a4.904 4.904 0 01-2.228-.616v.06a4.923 4.923 0 003.946 ",
        ~"4.827 4.996 4.996 0 01-2.212.085 4.936 4.936 0 004.604 3.417 9.867 9.867 0 01-6.102 2.105c-.39 ",
        ~"0-.779-.023-1.17-.067a13.995 13.995 0 007.557 2.209c9.053 0 13.998-7.496 13.998-13.985 ",
        ~"0-.21 0-.42-.015-.63A9.935 9.935 0 0024 4.59z"
    ];
icon_path(chevron_left) ->
    ~"M15 19l-7-7 7-7";
icon_path(chevron_right) ->
    ~"M9 5l7 7-7 7";
icon_path(search) ->
    ~"M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z".
