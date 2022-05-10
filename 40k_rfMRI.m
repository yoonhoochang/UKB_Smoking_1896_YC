% Load IDPs 
% slice the IDPs_resting_40k file into IDs (subs) and IDP values (IDP)
% 
load('/Users/yoonhoochang/Downloads/IDPs_rest_40k.txt');
subs = table2array(IDPsrest40k(:,1));
IDP = table2array(IDPsrest40k(:,2:end));

Apoints = [55 55+1485 55+(1485*2) 55+(1485*2)+21 55+(1485*2)+21+210 55+(1485*2)+21+(210*2)];
Apoints_dif = [Apoints(1) diff(Apoints)];
AMP100 = 1:Apoints(1);
FNET100 = Apoints(1)+1:Apoints(2);
PNET100 = Apoints(2)+1:Apoints(3);
AMP25 = Apoints(3)+1:Apoints(4);
FNET25 = Apoints(4)+1:Apoints(5);
PNET25 = Apoints(5)+1:Apoints(6);

% Load confound variables (
C = readtable('../../Desktop/confounds_40k.csv','FileType','text');
subs_pheno = table2array(C(:,1));
sex = table2array(C(:,2));
age = table2array(C(:,6));
center = table2array(C(:,4));
motion = table2array(C(:,8));
date_scan1 = table2array(C(:,3));
date_MH_OF = table2array(C(:,5));
clear C;

% Deconfound imaging data
addpath(genpath('~/Desktop/FSLNets'))

conf=[age nets_demean(age).^2 sex nets_demean(age).*nets_demean(sex) center motion];
conf=nets_inormal(conf); 
for v = 1:size(IDP,2)
    s = ~isnan(IDP(:,v)); 
    conf_tmp = nets_demean(conf(s,:)); 
    IDP(s,v) = nets_demean(IDP(s,v)-conf_tmp*(pinv(conf_tmp)*IDP(s,v)));
end

% Reduce PNETs to 6 ICA dimensions (as in Elliot et al)
load('/Users/yoonhoochang/Downloads/ICA6_weights.mat)
NETICA = nets_normalise([IDP(:,PNET25) IDP(:,PNET100)] * icaSdb'); clear icaSdb

subs_netica=[subs NETICA];
writematrix(subs_netica)
