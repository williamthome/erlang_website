-module(erlang_website_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([hero/1]).
-export([code_showcase/1]).
-export([carousel_button/1]).
-export([carousel_item/1]).
-export([what_is_erlang/1]).
-export([what_is_otp/1]).
-export([news_section/1]).
-export([news_article/1]).
-export([participate_section/1]).

mount(#{title := Title}, _Req) ->
    Bindings = #{id => ~"home"},
    Layout =
        {erlang_website_layout, render, main_content, #{
            title => Title
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="min-h-screen">
        {% Hero Section }
        {arizona_template:render_stateless(Module, hero, #{})}

        {% Main Content Grid }
        <main class="max-w-7xl mx-auto px-6 py-16 grid lg:grid-cols-2 gap-16">
            {% Left Column }
            <div class="space-y-16">
                {arizona_template:render_stateless(Module, news_section, #{})}
            </div>

            {% Right Column }
            <div class="space-y-16">
                {arizona_template:render_stateless(Module, what_is_erlang, #{})}
                {arizona_template:render_stateless(Module, what_is_otp, #{})}
                {arizona_template:render_stateless(Module, participate_section, #{})}
            </div>
        </main>
    </div>
    """).

hero(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <section class="relative bg-gradient-to-br from-gray-950 via-gray-900 to-gray-950 overflow-hidden">
        {% Background Pattern }
        <div class="absolute bg-grid-pattern opacity-5"></div>

        <div class="max-w-7xl mx-auto px-4 sm:px-6 py-16 sm:py-24 lg:py-32">
            <div class="grid lg:grid-cols-2 gap-16 items-center">
                {% Left Column - Content }
                <div class="text-center lg:text-left">
                    <h1 class="{[
                        ~"text-3xl sm:text-4xl md:text-5xl lg:text-7xl font-bold text-white ",
                        ~"mb-6 sm:mb-8 leading-tight"
                    ]}">
                        Practical
                        <span class="text-erlang-red">functional</span>
                        programming
                        <span class="{[
                            ~"block text-2xl sm:text-3xl md:text-4xl lg:text-5xl text-gray-300 ",
                            ~"mt-2 sm:mt-4"
                        ]}">
                            for a parallel world
                        </span>
                    </h1>

                    <p class="{[
                        ~"text-lg sm:text-xl text-gray-300 mb-8 sm:mb-10 leading-relaxed ",
                        ~"max-w-2xl px-4 sm:px-0"
                    ]}">
                        Build massively scalable soft real-time systems with requirements on
                        high availability using Erlang's fault-tolerant design.
                    </p>

                    <div class="flex flex-col sm:flex-row gap-4">
                        {arizona_template:render_stateless(erlang_website_components, link_button, #{
                            label => ~"Get Erlang/OTP 28",
                            href => ~"#",
                            icon => download
                        })}
                        {arizona_template:render_stateless(erlang_website_components, link_button, #{
                            label => ~"Learn More",
                            href => ~"#",
                            variant => outline
                        })}
                    </div>
                </div>

                {% Right Column - Code Showcase }
                <div class="relative overflow-hidden">
                    {arizona_template:render_stateless(Module, code_showcase, #{})}
                </div>
            </div>
        </div>
    </section>
    """").

code_showcase(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <div
        id="code-carousel"
        class="relative bg-gray-900 rounded-2xl border border-gray-700 overflow-hidden shadow-2xl"
    >
        {% Navigation Arrows }
        {arizona_template:render_stateless(Module, carousel_button, #{
            class => ~"left-4",
            icon => chevron_left,
            onclick => ~"prevExample()"
        })}
        {arizona_template:render_stateless(Module, carousel_button, #{
            class => ~"right-4",
            icon => chevron_right,
            onclick => ~"nextExample()"
        })}

        {% Terminal Header }
        <div class="flex items-center px-6 py-4 bg-gray-800 border-b border-gray-700">
            <div class="flex space-x-2">
                {arizona_template:render_list(fun(Color) ->
                    arizona_template:from_string(~"""
                    <div class="w-3 h-3 {Color} rounded-full"></div>
                    """)
                end, [~"bg-red-500", ~"bg-yellow-500", ~"bg-green-500"])}
            </div>
            <div
                id="file-name"
                class="flex-1 text-center text-gray-400 text-sm font-mono"
            >
                {maps:get(file, hd(examples()))}
            </div>
        </div>

        {% Code Content }
        <div class="relative overflow-hidden">
            <div class="carousel-slides" id="carousel-slides">
                {arizona_template:render_list(fun(#{index := Index} = Example) ->
                    OverflowClass = case Index of
                        0 -> ~"overflow-x-auto";
                        _ -> ~"overflow-hidden"
                    end,
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(Module, carousel_item, Example#{
                        overflow_class => OverflowClass
                    })}
                    """)
                end, examples())}
            </div>
        </div>

        {% Dots Navigation }
        <div class="flex justify-center space-x-2 p-4 bg-gray-850">
            {arizona_template:render_list(fun(#{index := Index} = _Example) ->
                arizona_template:from_string(~"""
                <button
                    type="button"
                    class="{[
                        ~"carousel-dot w-2 h-2 rounded-full transition-smooth cursor-pointer ",
                        case Index of 0 -> ~"bg-erlang-red active"; _ -> ~"bg-gray-600" end
                    ]}"
                    onclick="goToExample({Index})"
                ></button>
                """)
            end, examples())}
        </div>

        {% Label }
        <div class="px-3 sm:px-6 py-2 sm:py-4">
            <div class="flex justify-center">
                <div
                    id="example-label"
                    class="{[
                        ~"bg-erlang-red px-2 sm:px-3 py-1 rounded-full text-white ",
                        ~"text-xs font-semibold"
                    ]}"
                >
                    {maps:get(label, hd(examples()))}
                </div>
            </div>
        </div>
    </div>
    """").

carousel_button(Bindings) ->
    arizona_template:from_string(~"""
    <button
        type="button"
        class="{[
            ~"carousel-arrow absolute top-1/2 -translate-y-1/2 z-10 w-10 h-10 ",
            ~"bg-gray-800 hover:bg-gray-700 rounded-full flex items-center justify-center ",
            ~"text-white transition-smooth cursor-pointer ",
            arizona_template:get_binding(class, Bindings)
        ]}"
        onclick="{arizona_template:get_binding(onclick, Bindings)}"
    >
        {arizona_template:render_stateless(erlang_website_components, icon, #{
            icon => arizona_template:get_binding(icon, Bindings)
        })}
    </button>
    """).

carousel_item(Bindings) ->
    arizona_template:from_string(~""""
    <div
        class="{[
            ~"carousel-slide ",
            arizona_template:get_binding(overflow_class, Bindings)
        ]}"
        data-file="{arizona_template:get_binding(file, Bindings)}"
        data-label="{arizona_template:get_binding(label, Bindings)}"
    >
        <pre class="text-xs sm:text-sm leading-relaxed !m-0 h-full"><code class="language-erlang whitespace-pre">{
            html_encode(arizona_template:get_binding(code, Bindings))
        }</code></pre>
    </div>
    """").

what_is_erlang(_Bindings) ->
    section_card(#{
        title => ~"What is Erlang?",
        text => arizona_template:from_string(~"""
        <p>
            Erlang is a programming language used to build massively scalable
            soft real-time systems with requirements on
            <strong class="text-erlang-red">high availability</strong>.
            Some of its uses are in telecoms, banking, e-commerce,
            computer telephony and instant messaging.
        </p>
        <p>
            Erlang's runtime system has built-in support for
            <strong class="text-erlang-red">concurrency</strong>,
            <strong class="text-erlang-red">distribution</strong> and
            <strong class="text-erlang-red">fault tolerance</strong>.
        </p>
        """),
        label => ~"Erlang Quickstart",
        href => ~"#"
    }).

what_is_otp(_Bindings) ->
    section_card(#{
        title => ~"What is OTP?",
        text => arizona_template:from_string(~"""
        <p>
            OTP is set of Erlang libraries and design principles providing middle-ware to
            develop these systems. It includes its own distributed database, applications
            to interface towards other languages, debugging and release handling tools.
        </p>
        """),
        label => ~"Getting Started with OTP",
        href => ~"#"
    }).

section_card(Bindings) ->
    erlang_website_components:card(#{
        tag => ~"section",
        content => arizona_template:from_string(~""""
        <h2 class="text-3xl font-bold text-white mb-6">
            {arizona_template:get_binding(title, Bindings)}
        </h2>
        <div class="space-y-4 text-gray-300 leading-relaxed">
            {arizona_template:render_slot(arizona_template:get_binding(text, Bindings))}
            <div class="pt-4 text-erlang-red">
                {arizona_template:render_stateless(erlang_website_components, link, #{
                    href => arizona_template:get_binding(href, Bindings),
                    label => arizona_template:get_binding(label, Bindings),
                    icon => chevron_right,
                    extra_classes => ~"hover:text-erlang-red-light"
                })}
            </div>
        </div>
        """")
    }).

news_section(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <section>
        <h2 class="text-3xl font-bold text-white mb-8">News</h2>
        <div class="space-y-6">
            {arizona_template:render_list(fun(Article) ->
                arizona_template:from_string(~"""
                {arizona_template:render_stateless(Module, news_article, Article)}
                """)
            end, articles())}
        </div>
    </section>
    """").

news_article(Bindings) ->
    erlang_website_components:card(#{
        tag => ~"article",
        content =>
            arizona_template:from_string(~""""
            <h3 class="text-xl font-semibold text-erlang-red mb-2">
                {arizona_template:render_stateless(erlang_website_components, link, #{
                    label => arizona_template:get_binding(title, Bindings),
                    href => arizona_template:get_binding(href, Bindings),
                    extra_classes => ~"hover:text-erlang-red-light"
                })}
            </h3>
            <p class="text-gray-400 text-sm mb-3">
                {arizona_template:get_binding(date, Bindings)} by
                {arizona_template:get_binding(author, Bindings)}
            </p>
            <p class="text-gray-300 leading-relaxed">
                {arizona_template:get_binding(summary, Bindings)}
            </p>
            """")
    }).

participate_section(_Bindings) ->
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">
            Participate
        </h2>
        <div class="text-center">
            <div class="inline-flex items-center justify-center w-24 h-24 bg-white rounded-full mb-6">
                <img src="images/eef-logo.svg" alt="Erlang Ecosystem Foundation" class="w-16 h-16" />
            </div>
            <p class="text-gray-300 leading-relaxed mb-6">
                Join the Erlang Ecosystem Foundation and be part of the community
                that builds the future of concurrent, fault-tolerant systems.
            </p>
            {arizona_template:render_stateless(erlang_website_components, link_button, #{
                label => ~"Join the Community",
                href => ~"#"
            })}
        </div>
    </section>
    """).

% Internal functions

examples() ->
    [
        #{
            index => 0,
            file => ~"factorial.erl",
            label => ~"Functional programming",
            code => ~"""
            %% Pattern matching for control-flow
            fact(0) -> 1;
            fact(N) -> N * fact(N-1).

            %% Recursion to create loops
            > fact(10).
            3628800
            > [{I, fact(I)} || I <- lists:seq(1,10)].
            [{1, 1}, {2, 2}, {3, 6}, {4, 24}, {5, 120}, {6, 720},
             {7, 5040}, {8, 40320}, {9, 362880}, {10, 3628800}]
            """
        },
        #{
            index => 1,
            file => ~"high_order.erl",
            label => ~"Higher-order functions",
            code => ~"""
            %% Immutable variables
            > Fruits = ["banana","monkey","jungle"].
            ["banana","monkey","jungle"]
            %% Map values using stdlib functions
            > lists:map(fun string:uppercase/1, Fruits).
            ["BANANA","MONKEY","JUNGLE"]
            %% Fold over lists using custom functions
            > lists:foldl(fun(Str, Cnt) ->
                  string:length(Str) + Cnt
              end, 0, Fruits).
            18
            """
        },
        #{
            index => 2,
            file => ~"processes.erl",
            label => ~"Lightweight processes",
            code => ~"""
            %% Get own process id
            > Parent = self().
            <0.376.0>
            > Child = spawn(fun() ->
                  receive go -> Parent ! lists:seq(1,100) end
              end).
            <0.930.0>
            %% Send message to child
            > Child ! go.
            go
             %% Receive response from child
            > receive Reply -> Reply end.
            [1,2,3,4,5,6,7,8,9,10,11|...]
            """
        },
        #{
            index => 3,
            file => ~"parallel.erl",
            label => ~"Parallel map-reduce to find even numbers",
            code => ~"""
            -spec even(list(integer())) -> list(integer()).
            even(Numbers) ->
              mapreduce(Numbers, fun(Number) -> Number rem 2 == 0 end).
            mapreduce(Numbers, Function) ->
              Parent = self(),
              [spawn(fun() -> Parent ! {Number, Function(Number)} end)
               || Number <- Numbers],
              lists:flatten(
                [receive {Number, true} -> Number; _ -> [] end
                 || Number <- Numbers]).
            """
        }
    ].

articles() ->
    [
        #{
            title => ~"Erlang/OTP 28.0",
            href => ~"#",
            date => ~"May 21, 2025",
            author => ~"Henrik Nord",
            summary => ~"""
            Erlang/OTP 28.0 is a new major release with new features,
            improvements as well as a few incompatibilities.
            """
        },
        #{
            title => ~"Erlang/OTP 28 Highlights",
            href => ~"#",
            date => ~"May 20, 2025",
            author => ~"Isabell Huang",
            summary => ~"""
            Erlang/OTP 28 is finally here. This blog post will introduce the new
            features that we are most excited about.
            """
        },
        #{
            title => ~"Erlang/OTP 28.0 Release Candidate 4",
            href => ~"#",
            date => ~"May 05, 2025",
            author => ~"Björn Gustavsson",
            summary => ~"Erlang/OTP 28.0-rc4 is the fourth release candidate for OTP 28."
        }
    ].

%% HTML encoding helper for code examples
html_encode(Text) ->
    ReplacementList = [
        {"&", "\\&amp;"},
        {"<", "\\&lt;"},
        {">", "\\&gt;"},
        {"\"", "\\&quot;"},
        {"'", "\\&#39;"}
    ],
    lists:foldl(fun({Pattern, Replacement}, Acc) ->
        re:replace(Acc, Pattern, Replacement, [global, {return, binary}])
    end, Text, ReplacementList).
