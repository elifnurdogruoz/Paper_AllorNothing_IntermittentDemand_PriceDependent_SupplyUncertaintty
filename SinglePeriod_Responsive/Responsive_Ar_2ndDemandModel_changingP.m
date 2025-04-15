b1 = 1;
b2 = 1;
h = 0.01;
r = 0.1;
x = 0;
lamb = 1;
Q = 2;

% Create an empty matrix to store the results
results = zeros(0, 9);

for b1 = 1:1:2
    for b2 = 1:1:2
        for h = 0:0.125:1
            for r = 0:0.1:1
                for p_star= 0:0.5:5
                    for x = 0:1:10
                    Ar= r;
                    p_star = (lambertw(0, exp(-b2.*(Ar) - 1 + b1.*h)) - b1.*h + 1) / b1;
                    gpr = 1 / (1 + exp(b1.*p_star + b2.*(Ar)));
                    Gxpr = (p_star + h) .* gpr .* ((1 - exp(-lamb.*x))/lamb) - h.*x;
                    GxQpr = (p_star + h) .* gpr .* ((1 - exp(-lamb.*(x + r.*Q)))/lamb) - h.*(x + r.*Q);
                    fprintf('%d, %d, %0.1f, %0.3f, %d, %f, %f, %f, %f\n', b1, b2, r, h, x, p_star, gpr, Gxpr, GxQpr);
                    
                    % Append the current results to the matrix
                    results = [results; b1, b2, r, h, x, p_star, gpr, Gxpr, GxQpr];
                    end
                end
            end
        end
    end
end

% Write the results matrix to an Excel file
writematrix(results, 'Responsive_Ar_2nd_Demand_Model_changingP_r_16122023.xlsx');
% Alternatively, you can use writetable:
% writetable(array2table(results), 'output.xlsx');
