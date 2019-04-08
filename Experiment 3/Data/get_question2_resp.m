cd('D:\Box Sync\Projects\Contextual Cueing\Old\ContextualCueingNew_Expt9\Data');

mat_files = dir('*.mat');

sub_ids = [];
q2_resp = [];

for i = 1:length(mat_files);
    
   current_file = load(mat_files(i).name);
   

    
   sub_ids = [sub_ids current_file.subject_id];
   q2_resp = [q2_resp current_file.question2_resp];
    
end

out_table = table(sub_ids', q2_resp');

writetable(out_table, 'question2_resp.csv');