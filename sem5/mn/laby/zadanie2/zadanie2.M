function y = multinvhouse(V,x)
    [~, V_size] = size(V)
    y = x

    for i = 1:V_size
        y = householder(V(:, i), y);
    end
end

function y = householder(v, x)
    y = x - ((2 / norm(v)) * (v' * x) * v);
end

function x_norm = norm(x)
    x_norm = x' * x;
end
