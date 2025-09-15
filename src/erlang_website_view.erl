-module(erlang_website_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([hero/1]).
-export([hero_button/1]).
-export([code_showcase/1]).
-export([carousel_item/1]).
-export([what_is_erlang/1]).
-export([what_is_otp/1]).
-export([news_section/1]).
-export([news_article/1]).
-export([participate_section/1]).
-export([cta_link/1]).

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

hero_button(Bindings) ->
    arizona_template:from_string(~""""
    <a
        href="{arizona_template:get_binding(href, Bindings)}"
        class="{case arizona_template:get_binding(variant, Bindings) of
            ~"primary" -> ~"inline-flex items-center px-8 py-4 bg-erlang-red hover:bg-erlang-red-dark text-white font-semibold rounded-lg transition-smooth";
            ~"secondary" -> ~"inline-flex items-center px-8 py-4 border-2 border-gray-600 hover:border-erlang-red text-gray-300 hover:text-white font-semibold rounded-lg transition-smooth";
            _ -> ~"inline-flex items-center px-8 py-4 bg-gray-600 text-white font-semibold rounded-lg transition-smooth"
        end}"
    >
        {arizona_template:get_binding(label, Bindings)}
        {case arizona_template:get_binding(icon, Bindings) of
            ~"download" -> arizona_template:from_string(~"""
                <svg class="ml-2 w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4 4m0 0l-4-4m4 4V4"></path>
                </svg>
                """);
            ~"none" -> ~"";
            _ -> ~""
        end}
    </a>
    """").

hero(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <section class="relative bg-gradient-to-br from-gray-950 via-gray-900 to-gray-950 overflow-hidden">
        {% Background Pattern }
        <div class="absolute inset-0 bg-grid-pattern opacity-5"></div>

        <div class="max-w-7xl mx-auto px-4 sm:px-6 py-16 sm:py-24 lg:py-32">
            <div class="grid lg:grid-cols-2 gap-16 items-center">
                {% Left Column - Content }
                <div class="text-center lg:text-left">
                    <h1 class="text-3xl sm:text-4xl md:text-5xl lg:text-7xl font-bold text-white mb-6 sm:mb-8 leading-tight">
                        Practical
                        <span class="text-erlang-red">functional</span>
                        programming
                        <span class="block text-2xl sm:text-3xl md:text-4xl lg:text-5xl text-gray-300 mt-2 sm:mt-4">
                            for a parallel world
                        </span>
                    </h1>

                    <p class="text-lg sm:text-xl text-gray-300 mb-8 sm:mb-10 leading-relaxed max-w-2xl px-4 sm:px-0">
                        Build massively scalable soft real-time systems with requirements on
                        high availability using Erlang's fault-tolerant design.
                    </p>

                    <div class="flex flex-col sm:flex-row gap-4">
                        {arizona_template:render_list(fun(Button) ->
                            arizona_template:from_string(~"""
                            {arizona_template:render_stateless(Module, hero_button, Button)}
                            """)
                        end, [
                            #{
                                label => ~"Get Erlang/OTP 28",
                                href => ~"#",
                                variant => ~"primary",
                                icon => ~"download"
                            },
                            #{
                                label => ~"Learn More",
                                href => ~"#",
                                variant => ~"secondary",
                                icon => ~"none"
                            }
                        ])}
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
    Examples = [
        #{
            index => 1,
            file => ~"factorial.erl",
            label => ~"Functional programming",
            code => arizona_template:from_string(~"""
                <span class="text-gray-400">%% Pattern matching for control-flow</span>
                <span class="text-erlang-blue">fact</span>(<span class="text-yellow-400">0</span>) -> <span class="text-yellow-400">1</span>;
                <span class="text-erlang-blue">fact</span>(<span class="text-blue-400">N</span>) -> <span class="text-blue-400">N</span> * <span class="text-erlang-blue">fact</span>(<span class="text-blue-400">N</span>-<span class="text-yellow-400">1</span>).

                <span class="text-gray-400">%% Recursion to create loops</span>
                <span class="text-gray-500">></span> <span class="text-erlang-blue">fact</span>(<span class="text-yellow-400">10</span>).
                <span class="text-yellow-400">3628800</span>
                <span class="text-gray-500">></span> [\{<span class="text-blue-400">I</span>, <span class="text-erlang-blue">fact</span>(<span class="text-blue-400">I</span>)} || <span class="text-blue-400">I</span> <- <span class="text-erlang-blue">lists:seq</span>(<span class="text-yellow-400">1</span>,<span class="text-yellow-400">10</span>)].
                <span class="text-gray-300">[\{1, 1}, \{2, 2}, \{3, 6}, \{4, 24}, \{5, 120}, \{6, 720},
                 \{7, 5040}, \{8, 40320}, \{9, 362880}, \{10, 3628800}]</span>
                """)
        },
        #{
            index => 2,
            file => ~"even.erl",
            label => ~"Find even numbers",
            code => arizona_template:from_string(~"""
                <span class="text-gray-400">-spec even(In) -> Out</span>
                    <span class="text-gray-400">when In :: list(integer()),</span>
                         <span class="text-gray-400">Out :: list(integer()).</span>
                <span class="text-erlang-blue">even</span>(<span class="text-blue-400">Numbers</span>) ->
                    [<span class="text-blue-400">Number</span> || <span class="text-blue-400">Number</span> <- <span class="text-blue-400">Numbers</span>,
                     <span class="text-blue-400">Number</span> <span class="text-erlang-blue">rem</span> <span class="text-yellow-400">2</span> == <span class="text-yellow-400">0</span>].

                <span class="text-gray-500">></span> <span class="text-erlang-blue">even</span>([<span class="text-yellow-400">1</span>,<span class="text-yellow-400">2</span>,<span class="text-yellow-400">3</span>,<span class="text-yellow-400">4</span>,<span class="text-yellow-400">5</span>,<span class="text-yellow-400">6</span>,<span class="text-yellow-400">7</span>,<span class="text-yellow-400">8</span>,<span class="text-yellow-400">9</span>,<span class="text-yellow-400">10</span>]).
                <span class="text-gray-300">[2,4,6,8,10]</span>
                """)
        },
        #{
            index => 3,
            file => ~"concurrent.erl",
            label => ~"Concurrent processes",
            code => arizona_template:from_string(~"""
                <span class="text-gray-400">%% Spawn multiple processes</span>
                <span class="text-erlang-blue">mapreduce</span>(<span class="text-blue-400">Numbers</span>, <span class="text-blue-400">Function</span>) ->
                    <span class="text-blue-400">Parent</span> = <span class="text-erlang-blue">self</span>(),
                    [<span class="text-erlang-blue">spawn</span>(<span class="text-gray-400">fun</span>() -> <span class="text-blue-400">Parent</span> ! \{<span class="text-blue-400">Number</span>, <span class="text-blue-400">Function</span>(<span class="text-blue-400">Number</span>)} <span class="text-gray-400">end</span>) || <span class="text-blue-400">Number</span> <- <span class="text-blue-400">Numbers</span>],
                    <span class="text-erlang-blue">lists:flatten</span>(
                        [<span class="text-gray-400">receive</span> \{<span class="text-blue-400">Number</span>, <span class="text-erlang-blue">true</span>} -> <span class="text-blue-400">Number</span>; <span class="text-gray-400">_</span> -> [] <span class="text-gray-400">end</span> || <span class="text-blue-400">Number</span> <- <span class="text-blue-400">Numbers</span>]).

                <span class="text-gray-500">></span> <span class="text-erlang-blue">mapreduce</span>([<span class="text-yellow-400">1</span>,<span class="text-yellow-400">2</span>,<span class="text-yellow-400">3</span>,<span class="text-yellow-400">4</span>], <span class="text-gray-400">fun</span>(<span class="text-blue-400">N</span>) -> <span class="text-blue-400">N</span> <span class="text-erlang-blue">rem</span> <span class="text-yellow-400">2</span> == <span class="text-yellow-400">0</span> <span class="text-gray-400">end</span>).
                <span class="text-gray-300">[2,4]</span>
                """)
        }
    ],
    arizona_template:from_string(~""""
    <div class="relative bg-gray-900 rounded-2xl border border-gray-700 overflow-hidden shadow-2xl" id="code-carousel">
        {% Navigation Arrows }
        <button class="carousel-arrow absolute left-4 top-1/2 -translate-y-1/2 z-10 w-10 h-10 bg-gray-800 hover:bg-gray-700 rounded-full flex items-center justify-center text-white transition-smooth" onclick="prevExample()">
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7"></path>
            </svg>
        </button>
        <button class="carousel-arrow absolute right-4 top-1/2 -translate-y-1/2 z-10 w-10 h-10 bg-gray-800 hover:bg-gray-700 rounded-full flex items-center justify-center text-white transition-smooth" onclick="nextExample()">
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"></path>
            </svg>
        </button>

        {% Terminal Header }
        <div class="flex items-center px-6 py-4 bg-gray-800 border-b border-gray-700">
            <div class="flex space-x-2">
                <div class="w-3 h-3 bg-red-500 rounded-full"></div>
                <div class="w-3 h-3 bg-yellow-500 rounded-full"></div>
                <div class="w-3 h-3 bg-green-500 rounded-full"></div>
            </div>
            <div class="flex-1 text-center text-gray-400 text-sm font-mono" id="file-name">{maps:get(file, hd(Examples))}</div>
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
                    {arizona_template:render_stateless(Module, carousel_item, Example#{overflow_class => OverflowClass})}
                    """)
                end, Examples)}
            </div>
        </div>

        {% Dots Navigation }
        <div class="flex justify-center space-x-2 p-4 bg-gray-850">
            {arizona_template:render_list(fun(#{index := Index} = _Example) ->
                DotClass = case Index of
                    0 -> ~"carousel-dot active w-2 h-2 bg-erlang-red rounded-full transition-smooth";
                    _ -> ~"carousel-dot w-2 h-2 bg-gray-600 rounded-full transition-smooth"
                end,
                arizona_template:from_string(~"""
                <button class="{DotClass}" onclick="goToExample({integer_to_binary(Index)})"></button>
                """)
            end, Examples)}
        </div>

        {% Label }
        <div class="px-3 sm:px-6 py-2 sm:py-4">
            <div class="flex justify-center">
                <div class="bg-erlang-red px-2 sm:px-3 py-1 rounded-full text-white text-xs font-semibold" id="example-label">
                    {maps:get(label, hd(Examples))}
                </div>
            </div>
        </div>
    </div>
    """").

carousel_item(Bindings) ->
    arizona_template:from_string(~""""
    <div class="carousel-slide p-3 sm:p-6 {arizona_template:get_binding(overflow_class, Bindings)}" data-file="{arizona_template:get_binding(file, Bindings)}" data-label="{arizona_template:get_binding(label, Bindings)}">
        <pre class="text-xs sm:text-sm leading-relaxed"><code class="language-erlang whitespace-pre">{arizona_template:get_binding(code, Bindings)}</code></pre>
    </div>
    """").

what_is_erlang(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">What is Erlang?</h2>
        <div class="space-y-4 text-gray-300 leading-relaxed">
            <p>
                Erlang is a programming language used to build massively scalable soft
                real-time systems with requirements on <strong class="text-erlang-red">high availability</strong>.
                Some of its uses are in telecoms, banking, e-commerce, computer telephony and instant messaging.
            </p>
            <p>
                Erlang's runtime system has built-in support for <strong class="text-erlang-red">concurrency</strong>,
                <strong class="text-erlang-red">distribution</strong> and <strong class="text-erlang-red">fault tolerance</strong>.
            </p>
            <div class="pt-4">
                {arizona_template:render_stateless(Module, cta_link, #{
                    label => ~"Erlang Quickstart",
                    href => ~"#"
                })}
            </div>
        </div>
    </section>
    """).

what_is_otp(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">What is OTP?</h2>
        <div class="space-y-4 text-gray-300 leading-relaxed">
            <p>
                OTP is set of Erlang libraries and design principles providing middle-ware to
                develop these systems. It includes its own distributed database, applications
                to interface towards other languages, debugging and release handling tools.
            </p>
            <div class="pt-4">
                {arizona_template:render_stateless(Module, cta_link, #{
                    label => ~"Getting Started with OTP",
                    href => ~"#"
                })}
            </div>
        </div>
    </section>
    """).

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
            end, [
                #{
                    title => ~"Erlang/OTP 28.0",
                    href => ~"#",
                    date => ~"May 21, 2025",
                    author => ~"Henrik Nord",
                    summary => ~"Erlang/OTP 28.0 is a new major release with new features, improvements as well as a few incompatibilities."
                },
                #{
                    title => ~"Erlang/OTP 28 Highlights",
                    href => ~"#",
                    date => ~"May 20, 2025",
                    author => ~"Isabell Huang",
                    summary => ~"Erlang/OTP 28 is finally here. This blog post will introduce the new features that we are most excited about."
                },
                #{
                    title => ~"Erlang/OTP 28.0 Release Candidate 4",
                    href => ~"#",
                    date => ~"May 05, 2025",
                    author => ~"Björn Gustavsson",
                    summary => ~"Erlang/OTP 28.0-rc4 is the fourth release candidate for OTP 28."
                }
            ])}
        </div>
    </section>
    """").

news_article(Bindings) ->
    arizona_template:from_string(~"""
    <article class="bg-gray-900 rounded-2xl p-6 border border-gray-700 hover:border-gray-600 transition-smooth">
        <h3 class="text-xl font-semibold text-erlang-red mb-2">
            <a href="{arizona_template:get_binding(href, Bindings)}" class="hover:text-erlang-red-light transition-smooth">
                {arizona_template:get_binding(title, Bindings)}
            </a>
        </h3>
        <p class="text-gray-400 text-sm mb-3">
            {arizona_template:get_binding(date, Bindings)} by {arizona_template:get_binding(author, Bindings)}
        </p>
        <p class="text-gray-300 leading-relaxed">
            {arizona_template:get_binding(summary, Bindings)}
        </p>
    </article>
    """).

participate_section(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">Participate</h2>
        <div class="text-center">
            <div class="inline-flex items-center justify-center w-24 h-24 bg-white rounded-full mb-6">
                <img src="images/eef-logo.svg" alt="Erlang Ecosystem Foundation" class="w-16 h-16" />
            </div>
            <p class="text-gray-300 leading-relaxed mb-6">
                Join the Erlang Ecosystem Foundation and be part of the community
                that builds the future of concurrent, fault-tolerant systems.
            </p>
            {arizona_template:render_stateless(Module, hero_button, #{
                label => ~"Join the Community",
                href => ~"#",
                variant => ~"primary",
                icon => ~"none"
            })}
        </div>
    </section>
    """).

cta_link(Bindings) ->
    arizona_template:from_string(~"""
    <a href="{arizona_template:get_binding(href, Bindings)}" class="inline-flex items-center text-erlang-red hover:text-erlang-red-light transition-smooth">
        {arizona_template:get_binding(label, Bindings)}
        <svg class="ml-2 w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"></path>
        </svg>
    </a>
    """).
