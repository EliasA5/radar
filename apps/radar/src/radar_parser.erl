%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 21 July 2023
%%%-------------------------------------------------------------------

-module(radar_parser).

-export([parse_file/1, parse_line/1]).

parse_file(Filename) ->
  ParsedLines = parse_lines(read_lines(Filename)),
  erlang:iolist_to_binary(ParsedLines).

read_lines(Filename) ->
	case file:read_file(Filename) of
    {ok, Data} ->
      lists:map(fun binary_to_list/1,
                binary:split(Data, [<<"\n">>], [global]));
		{error, Err} ->
      throw({bad_file, Err})
	end.

parse_lines(Lines) ->
  lists:map(fun(Line) ->
                case parse_line(Line) of
                  nomatch ->
                    throw({bad_cmd, Line});
                  Bin ->
                    Bin
                end
            end, Lines).

parse_line(Line) ->
  Parsers = make_parsers(),
  apply_parsers(Parsers, Line).

make_parser_0_arg(Cmd, Opcode, Mapper) ->
  RegExp = " *(" ++ Cmd ++ ") *(\n|$)",
  fun(Str) ->
    case re:run(Str, RegExp) of
      nomatch -> nomatch;
      {match, _Cap} -> Mapper(Opcode)
    end
  end.

make_parser_1_arg(Cmd, Opcode, Mapper) ->
  RegExp = " *(" ++ Cmd ++ ") *(?<ARG1>(0|[1-9][0-9]*)) *(\n|$)",
  fun(Str) ->
    case re:run(Str, RegExp, [{capture, ['ARG1'], list}]) of
      nomatch -> nomatch;
      {match, [Arg1]} -> Mapper(Opcode, Arg1)
    end
  end.

make_parser_2_arg(Cmd, Opcode, Mapper) ->
  RegExp = " *(" ++ Cmd ++ ") *(?<ARG1>(0|[1-9][0-9]*)) *, *(?<ARG2>(0|[1-9][0-9]*)) *(\n|$)",
  fun(Str) ->
    case re:run(Str, RegExp, [{capture, all_names, list}]) of
      nomatch -> nomatch;
      {match, [Arg1, Arg2]} -> Mapper(Opcode, Arg1, Arg2)
    end
  end.

make_parsers() ->
  Make_Parser = fun
  (Cmd, Opcode, 0, Mapper) ->
      make_parser_0_arg(Cmd, Opcode, Mapper);
  (Cmd, Opcode, 1, Mapper) ->
      make_parser_1_arg(Cmd, Opcode, Mapper);
  (Cmd, Opcode, 2, Mapper) ->
      make_parser_2_arg(Cmd, Opcode, Mapper)
  end,

  [Make_Parser(Cmd, Opcode, Args, Mapper) ||
   {Cmd, Opcode, Args, Mapper} <- [{"inc_lcd", 1, 1, fun mapper_1/2},
                                   {"dec_lcd", 2, 1, fun mapper_1/2},
                                   {"rra_lcd", 3, 1, fun mapper_1/2},
                                   {"set_delay", 4, 1, fun mapper_1/2},
                                   {"clear_lcd", 5, 0, fun mapper_0/1},
                                   {"servo_deg", 6, 1, fun angle_mapper/2},
                                   {"servo_scan", 7, 2, fun first_smaller_deg/3},
                                   {"sleep", 8, 0, fun mapper_0/1}]
  ].

apply_parsers(_Parsers, []) ->
  <<>>;
apply_parsers([], _Line) ->
  nomatch;
apply_parsers([Parser | Parsers], Line) ->
  case Parser(Line) of
    nomatch ->
      apply_parsers(Parsers, Line);
    Bin ->
      Bin
  end.

mapper_0(Opcode) ->
  <<Opcode>>.

mapper_1(Opcode, Arg1) ->
  <<Opcode:8, (list_to_integer(Arg1)):8>>.

angle_mapper(Opcode, Angle) ->
  case list_to_integer(Angle) of
    Ang when Ang < 0 orelse Ang > 180 ->
      nomatch;
    Ang ->
      <<Opcode:8, Ang:8>>
  end.

first_smaller_deg(Opcode, Arg1, Arg2) ->
  Num1 = list_to_integer(Arg1),
  Num2 = list_to_integer(Arg2),
  case {Num1, Num2} of
    {_,_} when Num1 < 0 orelse
               Num1 > 180 orelse
               Num2 < 0 orelse
               Num2 > 180 orelse
               Num1 >= Num2 ->
      nomatch;
    _ ->
      <<Opcode:8, Num1:8, Num2:8>>
  end.

