Status
------
Usable, but it has 'issues' and is badly documented, cleanup pending.
Recomend using the original:

https://github.com/yariv/erlyweb/blob/master/src/erltl/erltl.erl


ErlTL
======

Erlang templating engine in the style of JSP and similar.


A rewrite of:
http://yarivsblog.blogspot.com/2006/10/introducing-erltl-simple-erlang.html
but adds (buggy) support for anti-quotation, eg:


    <% [ [""%> <% integer_to_list(I) %>, <%""] || I <- lists:seq(1,10) %>
  
which produces
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,


Usage
-----

    erltl:compile(File,Module,Opts).

Rebar
-----

    {deps,[{exemell, ".*", {git, "http://github.com/tiash/exemell.git", "master"}}]}.
    {plugins, [rebar_erltl]}.




