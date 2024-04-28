        update(Key, Val, [], [(Key, Val)]).
        update(Key, Val, [(Key, _)|Tail], [(Key, Val)|Tail]).
        update(Key, Val, [(Key1, Val1) | Tail], [(Key1, Val1)| Env1]) :- Key \= Key1, update(Key, Val, Tail, Env1).

        lookup(Key, [(Key, Val)|_], Val).
        lookup(Key, [(Key1, _)|Tail], Val) :- Key1 \= Key, lookup(Key, Tail, Val).

        eval_procedure(proc(X),Env2) :- eval_block(X, [], Env2).
        eval_block(blk(X), EnvIn, EnvOut) :- eval_statement_pipeline(X, EnvIn, EnvOut).
        eval_statement_pipeline(stmt_pipe(X,Z), EnvIn, EnvOut) :- eval_statement(X, EnvIn, EnvIn1), eval_statement_pipeline(Z, EnvIn1, EnvOut).
        eval_statement_pipeline(stmt_pipe(X), EnvIn, EnvOut) :- eval_statement(X, EnvIn, EnvOut).


        
        % Data Types
        eval_data_type(data_type_structure(X), EnvIn, R) :- eval_bool(X, EnvIn, R).
        eval_data_type(data_type_structure(X), EnvIn, R) :- eval_int(X, EnvIn, R).
        eval_data_type(data_type_structure(X), EnvIn, R) :- eval_charr(X, EnvIn, R).

            % Boolean Data type
            eval_bool(bool_structure(X), EnvIn, R) :- eval_bool_val(X, EnvIn, R).
            eval_bool(bool_structure(X), EnvIn, R) :- eval_conditional_logic(X, EnvIn, R).

            % literals
            eval_bool_val(boolean(true), EnvIn, EnvOut) :- update(bool, true, EnvIn, EnvOut).
            eval_bool_val(boolean(false), EnvIn, EnvOut) :- update(bool, false, EnvIn, EnvOut).

                % Conditional Expressions.
                eval_conditional_logic(cond_log(X), EnvIn, EnvOut) :- eval_logical_comparison(X, EnvIn, EnvOut).

                eval_conditional_logic(cond_log(X), EnvIn, R) :- eval_integer_comparison(X, EnvIn, R).
                eval_boolean_part(bool_part(X), EnvIn, EnvOut) :- eval_bool(X, EnvIn, EnvOut).
                eval_boolean_part(bool_part(X), EnvIn, R) :- eval_variable(X, EnvIn, Val), eval_bool(bool_structure(Val), EnvIn, R).

                    % And, Or, Not Gates TODO ADD SUPPORT FOR VAR
                    eval_logical_comparison(log_comp(X,Z), EnvIn, EnvOut) :- eval_boolean_part(X, EnvIn, EnvIn1), eval_boolean_part(Z, EnvIn1, EnvOut).
                    eval_logical_comparison(log_comp(X,Z), EnvIn, EnvOut) :- eval_boolean_part(X, EnvIn, EnvIn1), eval_boolean_part(Z, EnvIn1, EnvOut).
                    eval_logical_comparison(log_comp(X), EnvIn, EnvOut) :- eval_boolean_part(X, EnvIn, EnvOut).
                    % integer comparison
                    eval_integer_comparison(int_comp(X, Op, Z), EnvIn, R) :- eval_int(X, EnvIn, ValX), eval_int(Z, EnvIn, ValZ), eval_comparison_operator(Op, ValX, ValZ, R).

                    % Comparison Operator
                        eval_comparison_operator(comp_op(>), X, Z, _R) :- X > Z.
                        eval_comparison_operator(comp_op(<), X, Z, _R) :- X < Z.
                        eval_comparison_operator(comp_op(=), X, Z, _R) :- X =:= Z.


            % Integer Defination  % Loop ( remove loop )
            eval_int(int_structure(X), EnvIn, EnvOut) :- eval_numbers(X, EnvIn, EnvOut).
            eval_int(int_structure(X), EnvIn, EnvOut) :- eval_expression(X, EnvIn, EnvOut).
            % literals
            eval_numbers(num(X), _, X).
                % arithematic Expression ( interger functions )
                eval_expression_part(expr_part(X), EnvIn, EnvOut) :- eval_int(X, EnvIn, EnvOut).
                eval_expression_part(expr_part(X), EnvIn, EnvOut) :- eval_variable(X, EnvIn, EnvOut).
                eval_expression(expr(X, Operator, Z), EnvIn, Result) :- eval_expression_part(X, EnvIn, ValX), eval_expression_part(Z, EnvIn, ValZ), eval_operator(Operator, ValX, ValZ, EnvIn, Result).
                % Operator
               eval_operator(op(+), X, Z, _Env, Result) :- Result is X + Z.
               eval_operator(op(-), X, Z, _Env, Result) :- Result is X - Z.
               eval_operator(op(*), X, Z, _Env, Result) :- Result is X * Z.
               eval_operator(op(/), X, Z, _Env, Result) :- Result is X / Z.

              % String ( character Array )
              eval_charr(char(X), _, R) :- eval_string(X, _, R).
              %literals
              eval_string(str(X), _, X).

        % Statement types
        eval_statement(stmt(X),_,_) :- eval_null_statements(X).
        eval_statement(stmt(X), EnvIn, EnvOut) :- eval_print_statements(X, EnvIn, EnvOut).
        eval_statement(stmt(X), EnvIn, EnvOut) :- eval_assignment_statement(X, EnvIn, EnvOut).
        eval_statement(stmt(X), EnvIn, EnvOut) :- eval_conditional_statement(X, EnvIn, EnvOut).
        eval_statement(stmt(X), EnvIn, EnvOut) :- eval_loops(X, EnvIn, EnvOut).

            %   Null Statements
            eval_null_statements(nul_state()).

            %  Print Statements
            eval_print_statements(print_stmt(X), EnvIn, EnvOut) :- eval_data_type(X, EnvIn, _), EnvOut = EnvIn.
            eval_print_statements(print_stmt(X), EnvIn, EnvOut) :-  eval_variable(X, EnvIn, EnvOut).

            % assignment Statements
            eval_assignment_statement(assign_stmt(X, Value), EnvIn, EnvOut) :- eval_variable(X, EnvIn, EnvIn1), eval_data_type(Value, EnvIn, Val),  update(Var, Val, EnvIn, EnvOut) .
            eval_assignment_statement(assign_stmt(X, Value), EnvIn, EnvOut) :- eval_variable(X, EnvIn, EnvIn1), eval_expression(Value, EnvIn, Val), update(Var, Val, EnvIn, EnvOut) .
                % Variable
                eval_variable(variable_structure(X), EnvIn, Val) :- lookup(X, EnvIn, Val).

            % Conditional Statements
            eval_conditional_statement(cond_stmt(X,Y,Z), EnvIn, EnvOut) :- eval_bool(X, EnvIn, EnvIn1), eval_block(Y,EnvIn1, EnvIn2), eval_block(Z, EnvIn2, EnvOut).
            eval_conditional_statement(cond_stmt(X,Y,Z), EnvIn, EnvOut) :- eval_bool(X, EnvIn, EnvIn1), eval_statement_pipeline(Y, EnvIn1, EnvIn2), eval_statement_pipeline(Z, EnvIn2, EnvOut).

            % Loops
            eval_loops(loops(X,Y), EnvIn, EnvOut) :- eval_loop_part(X, EnvIn, EnvIn1), eval_block(Y, EnvIn1, EnvOut).
            eval_loops(loops(X,Y), EnvIn, EnvOut) :- eval_loopwith_part(X, EnvIn, EnvIn1), eval_block(Y, EnvIn1, EnvOut).
            eval_loops(loops(X,Y), EnvIn, EnvOut) :- eval_looprange_part(X, EnvIn, EnvIn1), eval_block(Y, EnvIn1, EnvOut).
                % while loop
                eval_loop_part(loop_part(X), EnvIn, EnvOut) :- eval_conditional_logic(X, EnvIn, EnvOut).
                % for loop
                eval_loopwith_part(loop_with(X,Y), EnvIn, EnvOut) :- eval_assignment_statement(X, EnvIn, EnvIn1), eval_conditional_logic(Y, EnvIn1, EnvOut).
                % range loop
                eval_looprange_part(loop_range(X,Z), EnvIn, EnvOut) :- eval_assignment_statement(X, EnvIn, EnvIn1),  eval_int(Z, EnvIn1, EnvOut).
