%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   sec817.pro                                 %
%                                              %
%  code to validate prolog flags.              %
%                                              %
%   Started 10:12AM  20/12/1995                %
%                                              %
%                                              %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%                                              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   The standard requires the following        %
%   flags                                      %
%                                              %
%   bounded                                    %
%     values:   true or false                  %
%     default: implementation defined          %
%     determines whether integers              %
%	are bounded or not.  not changeable    %
%                                              %	
%   max_integer                                %
%     default: implementation defined          %
%      not changeable                          %
%                                              %
%   min_integer                                %
%     default: implementation defined          %
%      not changeable                          %
%                                              %
%   integer_rounding_function                  %
%      values: down, toward_zero               %
%      default: implementation defined         %
%      not changeable                          %
%                                              %
%   char_conversion                            %
%      values  on, off                         %
%      default on                              %
%      changeable                              %
%                                              %
%   debug                                      %
%     values on, off                           %
%     default off                              %
%     changeable                               %
%                                              %
%   max_arity                                  %
%      default implementation defined          %
%      not changeable                          %
%                                              %
%   flag_unknown                               %
%      values  error, fail, warning            %
%      default error                           %
%      changeable                              %
%                                              %
%   double_quotes                              %
%      values: chars, codes, atom              %
%      default implmentation defined           %
%      changeable                              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   flags are manipulated by  two predicates   %
%                                              %
%   set_prolog_flag(+Flag, @Value)             %
%   current_prolog_flag(?Flag, ?Value)         %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

required_flags([bounded, max_integer, min_integer, 
                integer_rounding_function, char_conversion,
                debug, max_arity, unknown, double_quotes]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Test to see which flags are present
%   and which are aditional
%

find_flags :-
      setof(Flag, Val ^ current_prolog_flag(Flag, Val), Flags),
      required_flags(Rqd),
      log('Implemented flags:' ), log(Flags),log_nl,
      log('Required Flags: ' ), log(Rqd), log_nl .
      
find_flags :-
	log('No flags implemented'), log_nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test of halt/1 
%


test_halt1 :-
	test_true(halt(7)).


% multifile(validate/0).

validate :-
	test_817.


test_817 :-
        log('testing flags'), log_nl,
	find_flags,
        log_nl,
        log('testing  halt/1'), log_nl,
        test_halt1,
        log_nl,
        log('All tests completed).
        
