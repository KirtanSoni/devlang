
% DCG parse tree:
:- table bool/3,int/3.

% Program 
procedure(proc(X)) --> ['dev'], block(X), ['lang'].
block(blk(X)) --> ['{'], statement_pipeline(X), ['}'].
statement_pipeline(stmt_pipe(X,Z)) --> statement(X), [','], statement_pipeline(Z).  
statement_pipeline(stmt_pipe(X)) --> statement(X).


% Data Types
data_type(data_type_structure(X)) --> bool(X). 
data_type(data_type_structure(X)) --> int(X). 
data_type(data_type_structure(X)) --> charr(X). 

    % Boolean Data type 
    bool(bool_structure(X)) --> ['bool'],['('],bool_val(X),[')']. 
    bool(bool_structure(X)) --> conditional_logic(X).
    
    % literals
    bool_val(boolean(true))-->['true'].
    bool_val(boolean(false))-->['false'].

        % Conditional Expressions.
        conditional_logic(cond_log(X)) --> logical_comparison(X).
        conditional_logic(cond_log(X)) --> integer_comparison(X). 
        boolean_part(bool_part(X)) --> bool(X). 
        boolean_part(bool_part(X)) --> variable(X),{bool(X)}.

            % And, Or, Not Gates TODO ADD SUPPORT FOR VAR
            logical_comparison(log_comp(X,Z)) --> ['and'], ['('], boolean_part(X), [','], boolean_part(Z), [')'].
            logical_comparison(log_comp(X,Z)) --> ['or'], ['('], boolean_part(X), [','], boolean_part(Z), [')'].
            logical_comparison(log_comp(X)) --> ['not'], ['('], boolean_part(X) ,[')'].
            % integer comparison
            integer_comparison(int_comp(X,Y,Z)) --> int(X), comparison_operator(Y), int(Z).
                % Comparison Operator
                comparison_operator(comp_op(>)) --> ['>']. 
                comparison_operator(comp_op(<)) --> ['<'].
                comparison_operator(comp_op(=)) --> ['=='].

    
    % Integer Defination  % Loop ( remove loop )
    int(int_structure(X)) --> ['int'],['('], numbers(X) , [')']. 
    int(int_structure(X)) --> expression(X).
    % literals
    numbers(num(N_str)) --> [N_str], {  re_match("^[0-9]+$", N_str)}.
        % arithematic Expression ( interger functions )
        expression_part(expr_part(X)) --> int(X). 
        expression_part(expr_part(X)) --> variable(X),{int(X)}.
        expression(expr(X,Y,Z)) --> ['['], expression_part(X), operator(Y), expression_part(Z), [']'].
        % Operator
            operator(op(+)) --> ['+']. 
            operator(op(-)) --> ['-']. 
            operator(op()) --> ['']. 
            operator(op(/)) --> ['/'].

    % String ( character Array )
    charr(char(X)) --> ['charr'],['('], string(X), [')'].
        %literals
        string(str(X)) --> ['"'],[X],['"'],{re_match("^[A-Za-z0-9]*$", X)}.
        
% Statement types
statement(stmt(X)) --> null_statements(X).
statement(stmt(X)) --> print_statements(X).
statement(stmt(X)) --> assignment_statement(X).
statement(stmt(X)) --> conditional_statement(X).
statement(stmt(X)) --> loops(X).

    %   Null Statements
    null_statements(nul_state()) --> [' '].

    %  Print Statements
    print_statements(print_stmt(X)) --> ['tout'], ['('], data_type(X), [')'].
    print_statements(print_stmt(X)) --> ['tout'], ['('], variable(X), [')'].

    % assignment Statements 
    assignment_statement(assign_stmt(X,Z)) --> ['var'],variable(X),['='], data_type(Z). 
        % Variable 
        variable(variable_structure(I)) --> [I], {re_match("^[a-z]+$", I)}.

    % Conditional Statements
    conditional_statement(cond_stmt(X,Y,Z)) --> ['if'], ['('], bool(X), [')'], block(Y), ['otherwise'], block(Z).
    conditional_statement(cond_stmt(X,Y,Z)) --> ['?'], bool(X), [':'], statement_pipeline(Y), [':'], statement_pipeline(Z).

    % Loops
    loops(loops(X,Y)) --> loop_part(X), block(Y). 
    loops(loops(X,Y)) --> loopwith_part(X), block(Y). 
    loops(loops(X,Y)) --> looprange_part(X), block(Y).
        % while loop
        loop_part(loop_part(X)) --> ['loop'], ['('], conditional_logic(X), [')'].
        % for loop
        loopwith_part(loop_with(X,Y)) --> ['loopwith'], ['('], assignment_statement(X), [':'], conditional_logic(Y), [')'].
        % range loop
        looprange_part(loop_range(X,Z)) --> ['looprange'], ['('], assignment_statement(X),  int(Z), [')'].

