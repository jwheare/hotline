-module(hotline_utils).

-export([safe_utf8_to_unicode_list/1]).
-export([to_utf8_list/1]).

-define(UNI_REPL, 16#FFFD).
-define(UTF8_REPL, [239,191,189]).

safe_utf8_to_unicode_list(<<>>, {_, []}) ->
    [];
safe_utf8_to_unicode_list(<<>>, {_Orig, _Repl}) ->
    % lager:warning("Substituting replacement characters for invalid utf8 sequence: ~w Bad chars: ~w", [_Orig, lists:reverse(_Repl)]),
    [];
safe_utf8_to_unicode_list(Chars, {Orig, Repl}) ->
    Unicode = case unicode:characters_to_list(Chars) of
        {Err, Encoded, <<Invalid, Rest/binary>>} ->
            % There seems to be an erlang bug here where an old value for Rest
            % is reused unless we copy to a new variable
            % Might be related to optimisations detailed here:
            % http://www.erlang.org/doc/efficiency_guide/binaryhandling.html
            RealRest = <<Rest/binary>>,
            Next = case xmerl_ucs:is_latin1(Invalid) of
                true ->
                    % Handle as latin1, a subset of unicode
                    NextEncode = safe_utf8_to_unicode_list(RealRest, {Orig, Repl}),
                    [Invalid | NextEncode];
                false ->
                    NewRepl = [{Err, Invalid} | Repl],
                    NextEncode = safe_utf8_to_unicode_list(RealRest, {Orig, NewRepl}),
                    case Err of
                        incomplete ->
                            % Silently drop, usually appears at the end
                            NextEncode;
                        error ->
                            % Substitute the unicode replacement character
                            % http://en.wikipedia.org/wiki/Specials_(Unicode_block) #Replacement_character
                            [?UNI_REPL | NextEncode]
                    end
            end,
            Encoded ++ Next;
        Encoded when is_list(Encoded) ->
            Encoded
    end,
    {UnicodeChecked, _} = lists:foldl(fun(Codepoint, {Acc, Diacritics}) ->
        % unicode:characters_to_list can produce non unicode values
        case xmerl_ucs:is_unicode(Codepoint) of
            true ->
                % 3 combining diacritics in a row can cause issues alongside arabic text
                % https://news.ycombinator.com/item?id=6293824
                % Limit them to 2
                % http://www.fileformat.info/info/unicode/block/combining_diacritical_marks/index.htm
                case Codepoint >= 16#300 andalso Codepoint =< 16#36F of
                    true ->
                        case length(Diacritics) >= 2 of
                            true ->
                                % Had 2 in a row, silently drop
                                {Acc, Diacritics};
                            false ->
                                % Not had 2 yet, keep adding normally
                                {[Codepoint | Acc], [Codepoint | Diacritics]}
                        end;
                    false ->
                        % Not a combining diacritic, reset
                        {[Codepoint | Acc], []}
                end;
            false ->
                {[?UNI_REPL | Acc], Diacritics}
        end
    end, {[], []}, Unicode),
    lists:reverse(UnicodeChecked) ++ safe_utf8_to_unicode_list(<<>>, {Orig, Repl}).

safe_utf8_to_unicode_list(L) when is_list(L) ->
    safe_utf8_to_unicode_list(list_to_binary(L));
safe_utf8_to_unicode_list(Utf8) when is_binary(Utf8) ->
    safe_utf8_to_unicode_list(Utf8, {Utf8, []}).

to_utf8_list(Chars) ->
    Unicode = safe_utf8_to_unicode_list(Chars),
    binary_to_list(unicode:characters_to_binary(Unicode)).
