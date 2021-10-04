function x = kepler(M, epsilon, tol)
    a = -pi;
    b = pi;
    x = (a + b) / 2;

    while abs(b - a) > tol
        if kepler_fixed_point(M, epsilon, x) == 0
            return;
        end

        if sign(kepler_fixed_point(M, epsilon, x)) ~= sign(kepler_fixed_point(M, epsilon, a))
            b = x;
        else
            a = x;
        end
        x = (a + b) / 2;
    end
end

function val = kepler_fixed_point(M, epsilon, x)
    val = M + epsilon * sin(x) - x;
end
