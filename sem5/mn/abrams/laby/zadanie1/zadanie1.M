function x = fullsolve(b, eps)
    [N, ~] = size(b);
    A = create_A_matrix(N, eps);
    u = ones(N, 1);
    v = repmat(eps, N, 1);

    x = solve(A, u, v, b);
end

function A = create_A_matrix(N, eps)
    main_diag = repmat(6 - eps, 1, N);
    additional_diag = repmat(1 - eps, 1, N - 1)

    i = [1:N-1, 1:N, 2:N];
    j = [2:N, 1:N, 1:N-1];
    v = [additional_diag, main_diag, additional_diag];

    A = sparse(i, j, v, N, N);
end

function x = solve(A, u, v, b)
    solution_A_b = A \ b;
    solution_A_u = A \ u;

    x = solution_A_b - ((solution_A_u * (v' * solution_A_b)) / (1 + v' * solution_A_u))
end
