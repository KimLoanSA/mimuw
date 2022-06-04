function pp = hinterp(x, y, z)
    % oblicza kawałkami kubiczny interpolant hermitowski
    %   x - punkty interpolacji (wektor)
    %   y - wartości funkcji w punkach x_i (wektor)
    %   z - wartości pochodnej funkcji w punkach x_i (wektor)
    %
    %   pp - struktura typu `piecewise polynomial` z reprezentacją interpolatu

    arguments
        x (:, 1)
        y (:, 1)
        z (:, 1)
    end

    required_args = 3;
    if nargin ~= required_args
       error("za mało argumentów!");
    end

    [x_size, ~] = size(x);
    [y_size, ~] = size(y);
    [z_size, ~] = size(z);

    if x_size ~= y_size | x_size ~= z_size | y_size ~= z_size | x_size < 2
        error("niewłaściwe rozmiary wektorów wejściowych!");
    end

    if is_not_unique(x)
        error("x musi mieć unikalne wartości!");
    end

    pp = hinterp_for_valid_args(x, y, z);
end


function result = is_not_unique(vector)
    result = length(vector) ~= length(unique(vector));
end


function pp = hinterp_for_valid_args(x, y, z)
    [x_s, y_s, z_s] = sort_input_if_needed(x, y, z);
    coefs = get_coefs_for_input(x_s, y_s, z_s);

    pp = mkpp(x_s, coefs);
end


function [x, y, z] = sort_input_if_needed(x, y, z)
    input = [x, y, z];
    sorted_input = sortrows(input);

    if ~isequal(sorted_input, input)
        warning("x nie jest posortowane! sortowanie...");
    end

    x = sorted_input(:, 1);
    y = sorted_input(:, 2);
    z = sorted_input(:, 3);
end


function coefs = get_coefs_for_input(x, y, z)
    merged_input = get_merged_input(x, y, z);
    % będziemy się iterować po wierszach
    C = num2cell(merged_input, 2);
    % dla każdego "zestawu danych" (wiersza w macierzy) wywołujemy funkcję,
    % która na jej podstawie oblicza współczynniki
    cell_coefs = cellfun(@calculate_hermite_polynomial_coefs, C, 'UniformOutput', false);

    coefs = cell2mat(cell_coefs);
end


function coefs = calculate_hermite_polynomial_coefs(merged_input_cell)
    x_delta = merged_input_cell(2) - merged_input_cell(1);

    coefs = get_hermite_polynomial(...
        x_delta,...
        merged_input_cell(3),...
        merged_input_cell(4),...
        merged_input_cell(5),...
        merged_input_cell(6)...
   );
end


% oblicza wielomian hermitowski dla danej delty x (x_{i+1} - x{i})
% obliczamy dla delty ponieważ `mkpp` przesuwa przedziały do 0
% funkcje h:
% - https://en.wikipedia.org/wiki/Cubic_Hermite_spline
% - http://www.math.pitt.edu/~sussmanm/2070Fall07/lab_08/index.html
function p = get_hermite_polynomial(x_delta, y_i, y_ii, z_i, z_ii)
   [h_00, h_10, h_01, h_11] = get_hermite_polynomial_coefs_for_x_delta(x_delta);

   p = y_i .* h_00 + z_i .* h_10 + y_ii .* h_01 + z_ii .* h_11;
end


% oblicza współczynniki wielomianu hermitowskiego dla danej delty x (x_{i+1} - x_{i})
function [h_00, h_10, h_01, h_11] = get_hermite_polynomial_coefs_for_x_delta(x_delta)
    h_00 = [ 2 / (x_delta^3),     -3 / (x_delta^2),     0,      1];
    h_10 = [ 1 / (x_delta^2),     -2 /  x_delta,        1,      0];
    h_01 = [-2 / (x_delta^3),      3 / (x_delta^2),     0,      0];
    h_11 = [ 1 / (x_delta^2),     -1 /  x_delta,        0,      0];
end


% zwraca połączone wektory z wejscia, tak,
% aby kazdy wiersz zawierał wszystkie informacje potrzebne do obliczeń
%
% merged_input = [x_{i}, x_{i+1}, y_{i}, y_{i+1}, z_{i}, z_{i+1}]
function merged_input = get_merged_input(x, y, z)
    merged_input = [x(1:end-1), x(2:end), y(1:end-1), y(2:end), z(1:end-1), z(2:end)];
end
