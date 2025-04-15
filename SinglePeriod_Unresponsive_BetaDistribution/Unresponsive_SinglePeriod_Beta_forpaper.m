
h = 0.02;
lamb = 1;
K = 0;
Q = 2;
b1 = 1;
b2 = 1;

% Create arrays to store results
alf_values = [];
x_values = [];
p_values = [];
G_x_values = [];

for alf = 1:1:6
    for x = 0:1:10
        for p = 0:0.1:5
            bet = alf;

            fun_x = @(r) (1./(1 + (b1 * p + b2 * p.^2) .* (r))) .* (r.^(alf-1) .* (1-r).^(bet-1)) .* (gamma(alf+bet)./(gamma(alf) .* gamma(bet)));
            intg_x = integral(fun_x, 0, 1);
            G_x = (p + h) .* ((1 - exp(-lamb * x)) ./ lamb) .* intg_x - h .* x;

            % Store results in arrays
            alf_values = [alf_values; alf];
            x_values = [x_values; x];
            p_values = [p_values; p];
            G_x_values = [G_x_values; G_x];
        end
    end
end

% Create a table from the results
dataTable = table(alf_values, x_values, p_values, G_x_values, 'VariableNames', {'alf', 'x', 'p', 'G_x'});

% Specify the Excel file name
excelFileName = 'Unresponsive_SinglePeriod_ProfitFcn_Beta_h002.xlsx';

% Write the table to Excel
writetable(dataTable, excelFileName);

disp(['Data has been exported to ', excelFileName]);