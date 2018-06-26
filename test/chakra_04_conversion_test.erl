-module(chakra_04_conversion_test).

-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    ok.

blah_test() ->
    ok.

% erl -> js
%  - non 1-tuple
%  - non-list-props
%  - non-two-tuple property
%  - invalid key
%  - invalid value
%  - object property that's valid js but not a string
