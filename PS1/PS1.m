cd('E:\Labor and Population Economics\Problem Sets\PS1');

clear

% import the twins data
filedata = importdata('pubtwins.txt');
% create new variables in the base workspace from the fields in the
% imported data
for i = 1:size(filedata.colheaders, 2)
    assignin('base', genvarname(filedata.colheaders{i}),...
        filedata.data(:,i));
end
% clear the variables used to import the data
clear filedata i

% create some additional variables
age3 = age2.*age;
age4 = age3.*age;
educ2 = educ.^2;
educ_age=educ.*age;
female_age=female.*age;
female_educ=female.*educ;
white_age=white.*age;
white_educ=white.*educ;
% recode the "first twin" indicator to zero/one
first(isnan(first))=0;
% created a "rounded-off" education variable
educd=round(educ);

