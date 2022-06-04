function [a, b, c, d, e] = orbit(x, y)
result = get_result(x, y);

a = result(1);
b = result(2);
c = result(3);
d = result(4);
e = result(5);
end

  function result = get_result(x, y)
A = create_A(x, y);
b = x .* x;

[Q, R] = qr(A);
Q_1 = Q(:, 1:5);
R_1 = R(1:5, :);

b_1 = Q_1' * b;
result = R_1 \ b_1;
end

  function A = create_A(x, y)
N = size(x);

y_square = y .* y;
x_y = x .* y;

A = [y_square, x_y, x, y, ones(N)];
end