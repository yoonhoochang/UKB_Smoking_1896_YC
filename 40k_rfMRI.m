clear all; close all; clc

% Load IDPs 
load('/Users/yoonhoochang/Box/UKB Phenotypes/regression_results/Final_results/IDPs_45k_rest.txt');

subs = table2array(IDPs45krest(2:end,1));
IDP = table2array(IDPs45krest(2:end,2:end));

Apoints = [55 55+1485 55+(1485*2) 55+(1485*2)+21 55+(1485*2)+21+210 55+(1485*2)+21+(210*2)];
Apoints_dif = [Apoints(1) diff(Apoints)];
AMP100 = 1:Apoints(1);
FNET100 = Apoints(1)+1:Apoints(2);
PNET100 = Apoints(2)+1:Apoints(3);
AMP25 = Apoints(3)+1:Apoints(4);
FNET25 = Apoints(4)+1:Apoints(5);
PNET25 = Apoints(5)+1:Apoints(6);

% Load nuisance variables
C = readtable('../../Desktop/UKB_40K/confounds_ica.txt','FileType','text');

subs_pheno = table2array(C(:,1));
sex = table2array(C(:,2));
age = table2array(C(:,14));
center = table2array(C(:,9));
motion = table2array(C(:,18));
date_initial = table2array(C(:,3)); %nowhere%
date_scan1 = table2array(C(:,5));
date_scan2 = table2array(C(:,6)); %nowhere%
date_MH_OF = table2array(C(:,11));
clear C;

% Deconfound imaging data
addpath(genpath('~/Desktop/FSLNets'))
conf=[age nets_demean(age).^2 sex nets_demean(age).*nets_demean(sex) center motion];
missing_conf = find(isnan(sum(conf,2))==1);
conf(missing_conf,:) = [];
IDP(missing_conf,:) = [];
subs(missing_conf,:) = []; subs_pheno(missing_conf,:) = [];
conf=nets_inormal(conf); 
for v = 1:size(IDP,2)
    s = ~isnan(IDP(:,v)); 
    conf_tmp = nets_demean(conf(s,:)); 
    IDP(s,v) = nets_demean(IDP(s,v)-conf_tmp*(pinv(conf_tmp)*IDP(s,v)));
end

% Reduce PNETs to 6 ICA dimensions (as in Elliot et al)
load ICA6_weights.mat
NETICA = nets_normalise([IDP(:,PNET25) IDP(:,PNET100)] * icaSdb'); clear icaSdb
 
subs_netica=[subs NETICA];
writematrix(subs_netica,'subs_netica_40k.txt')
