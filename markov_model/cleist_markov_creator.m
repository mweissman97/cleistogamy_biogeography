all_crates = [0:0.01:1.0]; %cleistogamy transition rate
all_erates = [0:0.01:1]; %extinction rate for complete cleistogamous species

len = length(all_crates)*length(all_erates);

df = zeros(len, 7);

 ll = 1;
for eidx = 1:length(all_erates)
    er = all_erates(eidx);
    for cidx = 1:length(all_crates)
        cr = all_crates(cidx);
        %construct transition matrix
        T = [(1-cr), 0, er;
            cr, (1-cr), 0;
            0, cr, (1-er)];
        
        %find stationary distribution
        bigmat = T^100;
        
        df(ll,1) = cr;
        df(ll,2) = er;
        df(ll,3) = bigmat(1,1); %neither
        df(ll,4) = bigmat(2,1); %dim
        df(ll,5) = bigmat(2,1)/(1-bigmat(1,1)); %dim, but just out of the two BH
        df(ll,6) = bigmat(3,1); %com
        df(ll,7) = bigmat(3,1)/(1-bigmat(1,1)); %com, but just out of the two bh
        
        ll = ll+1;
    end
end

out = array2table(df); 
out.Properties.VariableNames = ["cleist", "ext", "neither_freq", "dim_freq", "dim_bh_norm", "com_freq", "com_bh_norm"]; 
writetable(out, "matlab_markov_out.csv");
