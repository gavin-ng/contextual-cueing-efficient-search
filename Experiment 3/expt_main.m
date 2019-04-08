% Screen('Preference', 'SkipSyncTests', 1 );

%%  Contextual cueing in efficient parallel search
%   Created by Gavin Ng
%   Last edit: 11/12/2017
%
%   Contextual cueing with lures
%   5 set sizes: 4, 10, 20, 32 + target only
%   3 different displays for each set size except for target-only
%   Total: 12 + 1 repeated displays
%
%   Displays repeat each block
%   Target direction is always randomly assigned, but location is always
%   fixed



%%%%%%%%%%%%%%%%%% INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%%
% start timer
tic
rand('state',sum(100*clock));
warning off MATLAB:DeprecatedLogicalAPI	  

subject_id = input('Enter Subject Number:    ');

mkdir(int2str(subject_id));

KbName('UnifyKeyNames');

HideCursor;

global Xcentre;
global Ycentre;
global cx;
global cy;

    
%% Reserving memory space for large variables
background = [0 0 0];
% [window, rect] = Screen('OpenWindow', max(Screen('Screens')), background);
[window, rect] = Screen('OpenWindow', max(Screen('Screens')), background, [0, 0, 1024, 768]);


% To make the window a smaller size for debugging, uncomment next line
% [window, rect] = Screen('OpenWindow', 0, background, rect);

Screen('TextSize', window, 40);


%% Stimuli Stuff %%
%%%%%%%%%%%%%%%%%%%%%

% enable Alpha-Blending
Screen('BlendFunction',window,GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);


% images to make textures
[tar_l,~,alpha] = imread('ltri.png');
tar_l(:,:,4)=alpha(:,:);
[tar_r,~,alpha] = imread('rtri.png');
tar_r(:,:,4)=alpha(:,:);
[l1,~,alpha] = imread('odiamond.png');
l1(:,:,4)=alpha(:,:);


t_l = Screen('MakeTexture',window,tar_l);
t_r = Screen('MakeTexture',window,tar_r);
l_1 = Screen('MakeTexture',window,l1);

%% Display parameters %%
%%%%%%%%%%%%%%%%%%%%%%%%

% Sizes and colours
XLeft=rect(RectLeft);	
XRight=rect(RectRight);
YTop=rect(RectTop);		
YBottom=rect(RectBottom);
Xcentre=XRight./2; 		
Ycentre=YBottom./2;

white = [255, 255, 255];
black = [0, 0, 0];

% offset and jitter for stimuli
offset = 0;
jitter = 20;

% response keys
escKey=KbName('ESCAPE');
startKey=KbName('=+');
LeftKey=KbName('LeftArrow');
RightKey=KbName('RightArrow');
returnKey=13;
deleteKey=8;


% timing
refresh_rate=85;
bit=(1/refresh_rate)/2;
resp_time=5;
fix_time=0.6;
% fix_time = 0.1;
onset_delay = 0.45;
disp_time= 5;
recognition_time = 10000;
% disp_time = 0.1;
inter_trial=1;
rest_time=30;   %rest time between blocks
% 
% % % % 
%speed run
% refresh_rate=85;
% bit=(1/refresh_rate)/2;
% resp_time=0.1;
% fix_time=0.1;
% % fix_time = 0.1;
% onset_delay = 0.1;
% disp_time= 0.1;
% recognition_time = 0.1
% % disp_time = 0.1;
% inter_trial=0.1;
% rest_time=0.1;   %rest time between blocks


%% end speed run
exit_flag = 0;

total_blocks = 25;
total_trials = total_blocks * 26;





% variables to store data
Trial = ones(total_trials, 1) * (-1);
T_ID = ones(total_trials, 1) * (-1);
D_ID = ones(total_trials, 1) * (-1);
D_setsize = ones(total_trials, 1) * (-1);
Resp = ones(total_trials, 1) * (-1);
Error = ones(total_trials, 1) * (-1);
RT = ones(total_trials, 1) * (-1);
Fix_onset = ones(total_trials, 1) * (-1);
Disp_onset = ones(total_trials, 1) * (-1);
T_xloc = ones(total_trials, 1) * (-1);
T_yloc = ones(total_trials, 1) * (-1);
Block = ones(total_trials, 1) * (-1);
Eccentricity = ones(total_trials, 1) * (-1);






%% Prepare displays %%
%%%%%%%%%%%%%%%%%%%%%%

design = repmat(struct('block',0,'repeat', 0,'tid',1, 'tloc', 0, 'd_setsize',0,...
        'quad1',0,'quad2',0,'quad3',0,'quad4',0,...
        'loc1',0,'loc2',0,'loc3',0,'loc4',0,'loc5',0,'loc6',0,'loc7',0,'loc8',0,'loc9',0,...
        'loc10',0,'loc11',0,'loc12',0,'loc13',0,'loc14',0,'loc15',0,'loc16',0,'loc17',0,'loc18',0,...
        'loc19',0,'loc20',0,'loc21',0,'loc22',0,'loc23',0,'loc24',0,'loc25',0,'loc26',0,'loc27',0,...
        'loc28',0,'loc29',0,'loc30',0,'loc31',0,'loc32',0,'loc33',0,'loc34',0,'loc35',0,'loc36',0),1,1);

design = struct2table(design);




%% create location array for target only

% create a table with 36 locations (corresponding to grid1)
target_table = repmat(design,36,1);
% make half the target ids == 1
target_ids = zeros(36,1);
target_ids(1:18,1) = 1;
% randomize target location
loct = zeros(1,36);
target_iloc = randperm(numel(loct));
% randomize target id
t_id = vertcat(zeros(18,1), ones(18,1));
t_id_rand = randperm(length(t_id));
% update target_table
for block = 1:height(target_table)
    target_table.tloc(block) = target_iloc(block);
    target_table.tid(block) = t_id(t_id_rand(block));
    target_table.repeat(block) = 0;
            % negative indicates that it is a target
            % first digit indicates orientation (ignore in this case)
            % 2nd and 3rd indicate the x jitter
            % 4th indicates positive (1) or negative (0) jitter
            % 5th and 6th indicate y jitter
            % 7th indicates positive (1) or negative (0) jitter
    target_table{block, target_iloc(block)+9} = - ((randi([1 4], 1, 1) * 1000000) + ...
                    (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
                    (randi([0 jitter], 1, 1) * 10) + round(rand(1))) * 10 + 9;
    % set one target to be the repeated target (block == 0)
    target_table.block(block) = block-1;
end

% set first target's repeat status to 1
target_table.repeat(1) = 1;

% get the location of the repeated target  (to exclude it from other
% displays)
repeated_target = target_table.tloc(1);

%% create location array for repeated distracor displays (12 in total) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initialize stuff
repeat_table = repmat(design,12,1);
setsizes = [3, 9, 19, 31];
candidate_setsize = 8;
repeat_locs = zeros(12, 36);
new_cand_conf = [];
count = 0;



for ss = setsizes
    

    % we need 6 different repeat displays per set size
    % create all_conf to store these 6 displays
    all_conf= zeros(6,36);
    % set duplicate check to 1 for while loop
    has_duplicates = 1;

    for display = 1:3
        % while quad < limit & while there are duplicate trials
        while has_duplicates

            % re-initialize to 0
            all_conf(display,:) = zeros(1,36);
            % randomize target locations
            tloc = randsample(setdiff(1:36, repeated_target), 1);

            % check against previous repeated displays
            % resample if there is a duplicate
            while ismember(tloc, repeat_table.tloc)
                tloc = randsample(setdiff(1:36, repeated_target), 1);
            end

            
            % conf refers to configuration
            % concatenate all the locations together and sort
            lure_conf = sort(randsample(setdiff(1:36, tloc), ss));
%             cand_conf = sort(randsample(setdiff(1:36, [lure_conf tloc]), candidate_setsize-1));
%             cand_conf = sort([cand_conf tloc]);
            
            

            % this will be a 7 digit number
            % first digit indicates orientation
            % 2nd and 3rd indicate the x jitter
            % 4th indicates positive (1) or negative (0) jitter
            % 5th and 6th indicate y jitter
            % 7th indicates positive (1) or negative (0) jitter
%             for pos = cand_conf
%                 all_conf(display, pos) = (randi([1 4], 1, 1) * 1000000) + ...
%                     (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
%                     (randi([0 jitter], 1, 1) * 10) + round(rand(1));
%             end
            
            % this will be an 8 digit number
            % same logic as above, but the last digit (9) indicates that
            % this is a lure
            
            for pos = lure_conf
                all_conf(display, pos) = ((randi([1 4], 1, 1) * 1000000) + ...
                    (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
                    (randi([0 jitter], 1, 1) * 10) + round(rand(1))) * 10 + 9;
            end
    
            
            % same logic as above, but now we make it negative so that
            % we know that it is a target
            all_conf(display, tloc) = - ((randi([1 4], 1, 1) * 1000000) + ...
                    (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
                    (randi([0 jitter], 1, 1) * 10) + round(rand(1)));

        
            
            

            % check duplicates
            [x, y, z] = unique(all_conf(1:display,:), 'rows', 'first');
            has_duplicates = size(x,1) < size(all_conf(1:display,:),1);
            
            
        end



        % update repeat_table
        % position columns start from column 10 (other columns before
        % that e.g. trial number)
        repeat_table{display+count, 10:end} = all_conf(display,:);
        repeat_table.repeat(display+count) = 1;
        repeat_table.tloc(display+count) = tloc;
        repeat_table.d_setsize(display+count) = ss;

        
        % get quad values and assign
       
        quad1 = sum(abs(all_conf(display, 1:9)));
        quad2 = sum(abs(all_conf(display, 10:18)));
        quad3 = sum(abs(all_conf(display, 19:27)));
        quad4 = sum(abs(all_conf(display, 28:36)));
        
        repeat_table.quad1(display+count) = quad1;
        repeat_table.quad2(display+count) = quad2;
        repeat_table.quad3(display+count) = quad3;
        repeat_table.quad4(display+count) = quad4;

        % create an "old_check" table for checking the new displays
        % against (in the next block of code)
        old_check(display+count, :) = all_conf(display,:);
        
        

        % reset checking parameters to enter while loop again
        has_duplicates = 1;            

    end
    count=count+3;
            

end
    

%% Create new displays %%
%%%%%%%%%%%%%%%%%%%%%%%%%

% logic for this block of code is the same as the previous block of code


new_table = repmat(design, total_blocks* size(repeat_table,1), 1); 
count = 0;

% get 12 new target locations
new_tlocs = randsample(setdiff(1:36, vertcat(repeat_table.tloc, repeated_target)),12);




for block = 1:total_blocks + 1 % create 1 more set for recognition test
    % create a copy of old_check
    old_check_1 = old_check;
    
    
    % counter for target location
    tcount = 1;
    % shuffle the target locations
    new_tlocs_rand = new_tlocs(randperm(length(new_tlocs)));
    
    for ss = setsizes
        
        
        
        
        % we need 6 different new displays per set size per block
        % create new_conf to store these displays
        
        new_all_conf = zeros(6,36);
        
        % set duplicate check to TRUE for while loop
        has_duplicates = 1;
        

        for display = 1:3
            
            while has_duplicates
                % re-initialize to 0
                new_all_conf(display,:) = zeros(1,36);
                new_tloc = new_tlocs_rand(tcount); 
                
                % conf refers to configuration
                % concatenate all the locations together and sort
                new_lure_conf = sort(randsample(setdiff(1:36, new_tloc), ss));
%                 new_cand_conf = sort(randsample(setdiff(1:36, [new_lure_conf new_tloc]), candidate_setsize-1));
%                 new_cand_conf = sort([new_cand_conf new_tloc]);
                

                % set candidate locations to 7 digit

%                 for pos = new_cand_conf
%                     new_all_conf(display, pos) = (randi([1 4], 1, 1) * 1000000) + ...
%                     (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
%                     (randi([0 jitter], 1, 1) * 10) + round(rand(1));
%                 end

                % set lure conditions to 8 digit
                
                for pos = new_lure_conf
                    new_all_conf(display, pos) = ((randi([1 4], 1, 1) * 1000000) + ...
                    (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
                    (randi([0 jitter], 1, 1) * 10) + round(rand(1))) * 10 + 9;
                end
          
            
                % assign target as negatve for displaying later
                new_all_conf(display, new_tloc) = - ((randi([1 4], 1, 1) * 1000000) + ...
                    (randi([0 jitter], 1, 1) * 10000) + (round(rand(1)) * 1000) + ...
                    (randi([0 jitter], 1, 1) * 10) + round(rand(1)));
                
                % check duplicates against both new and repeated displays
                [x, y, z] = unique(new_all_conf(1:display,:), 'rows', 'first');
                has_duplicates = size(x,1) < size(new_all_conf(1:display,:),1);
                
                old_check_1 = vertcat(old_check_1(1:12,:), new_all_conf(1:display,:));
                [x, y, z] = unique(old_check_1(1:size(old_check,1),:), 'rows', 'first');
                has_duplicates = size(x,1) < size(old_check(1:size(old_check,1),:),1);
            end
            
            
            
            % update new_table
            % position columns start from column 6 
            % (there are other columns before that
            % e.g. trial number)
            
            new_table{display+count, 10:end} = new_all_conf(display,:);
            new_table.repeat(display+count) = 0;
            new_table.tloc(display+count) = new_tloc;
            new_table.d_setsize(display+count) = ss;
            new_table.block(display+count) = block;
            
            % get quad values and assign
       
            quad1 = sum(abs(new_all_conf(display, 1:9)));
            quad2 = sum(abs(new_all_conf(display, 10:18)));
            quad3 = sum(abs(new_all_conf(display, 19:27)));
            quad4 = sum(abs(new_all_conf(display, 28:36)));

            new_table.quad1(display+count) = quad1;
            new_table.quad2(display+count) = quad2;
            new_table.quad3(display+count) = quad3;
            new_table.quad4(display+count) = quad4;

            
            % reset checking parameters to enter while loop again
            has_duplicates = 1;
            
            % increase target counter
            tcount = tcount + 1;

        end
        
        % increase target counter and counter by 3 since each loop creates 66 displays
       
        count = count+3;
        
        
        
            
        
        
        
    end
    
end

%% Dataframe for recognition test

recog_0_tloc = randsample(setdiff(1:36, vertcat(repeat_table.tloc, repeated_target, new_tlocs')), 1);

target_table_temp = [target_table(target_table.repeat == 1,:); target_table(target_table.tloc == recog_0_tloc, :)];

recognition_table_temp = [repeat_table; new_table(new_table.block==(total_blocks+1), :); target_table_temp];
recog_tids = vertcat(zeros(height(recognition_table_temp)/2, 1), ones(height(recognition_table_temp)/2, 1));
recog_tids = Shuffle(recog_tids);
% recognition_table_temp.tid(:) = 1;
% recognition_table = [recognition_table_temp];

recognition_table_temp_2 = recognition_table_temp;
recognition_table_temp_2.tid(:) = recog_tids;
recognition_table_temp_2.block(:) = 1;


for b = 2:4
    
    recog_tids = abs(recog_tids - 1);
    
   recognition_table_temp_1 = recognition_table_temp_2(recognition_table_temp_2.block==b-1,:);
   recognition_table_temp_1.block(:) = b;
   recognition_table_temp_1.tid(:) = recog_tids;
   
   recognition_table_temp_2 = [recognition_table_temp_2; recognition_table_temp_1];
   
end

recognition_table = recognition_table_temp_2(recognition_table_temp_2.block == 1, :);
recognition_table = recognition_table(randperm(26),:);

for b = 2:4
    
   recognition_table_temp_3 = recognition_table_temp_2(recognition_table_temp_2.block == b, :);
   
   recognition_table = [recognition_table; recognition_table_temp_3(randperm(height(recognition_table_temp_3)), :)];
    
    
    
end





% variables to store data
Trial_recog = ones(height(recognition_table), 1) * (-1);
% T_ID = ones(total_trials, 1) * (-1);
% D_ID = ones(total_trials, 1) * (-1);
% D_setsize = ones(total_trials, 1) * (-1);
Resp_recog = ones(height(recognition_table), 1) * (-1);
Hit_recog = ones(height(recognition_table), 1) * (-1);
RT_recog = ones(height(recognition_table), 1) * (-1);
Confidence_recog = ones(height(recognition_table), 1) * (-1);
% Fix_onset = ones(total_trials, 1) * (-1);
% Disp_onset = ones(total_trials, 1) * (-1);
% T_xloc = ones(total_trials, 1) * (-1);
% T_yloc = ones(total_trials, 1) * (-1);
% Block = ones(total_trials, 1) * (-1);
% Eccentricity = ones(total_trials, 1) * (-1);



%% START ACTUAL EXPT %%
%%%%%%%%%%%%%%%%%%%%%%%

        
%% Instructions %%
%%%%%%%%%%%%%%%%%%



inst_1 = ['Welcome! \n\n'...
    'This experiment requires you to visually search for a target object\n\n'...
    'and make a decision about its details. \n\n'...
    'The target is a red letter T and it would appear among an array of\n\n'...
    'distractors of other colored shapes. \n\n'...
    'Your task is to decide to which direction\n\n'...
    'the red letter T is rotated. \n\n'...
    'You will see examples of both cases on the next screen. \n\n'];

inst_2 = ['Here are the two possible target images:\n\n\n\n\n\n\n\n'...
    'Press the left arrow key with your left index finger \n\n when the T is rotated to the left, \n\n'...
    'and the right arrow key with your right index finger \n\n when it''s pointing to the right. \n\n'...
    'There will always be ONE and ONLY ONE target in each trial. \n\n'...
    'Please try to respond as quickly and as accurate as possible. \n\n'...
    'If you have any questions please ask the experimenter now. '];

inst_3 = ['The best strategy for this task, and the one that we want you to use in this study, \n\n'...
    'is to be as receptive as possible and let the unique item "pop" into your mind as you \n\n'...
    'look at the screen. The idea is to let the display and your intuition determine \n\n'...
    'your response. Sometimes people find it difficult or strange to tune into their \n\n'...
    '"gut feelings", but we would like you to try your best. \n\n'...
    'Try to respond as quickly and accurately as you can while using this strategy. \n\n'...
    'Remember, it is very critical for this experiment that you \n\n'...
    'let the unique item just �pop� into your mind.'];

practice_inst = ['We will first start with some practice trials. \n\n\n\n'...
    'Hit the LEFT or RIGHT arrow key to begin.'];

test_start = ['Now the test phase will begin. \n\n'...
    'Your response data will be recorded for our analysis. \n\n\n\n'...
    'Hit the LEFT or RIGHT arrow key to begin.'];

rest_inst = ['Please take a short break. Rest your eyes and hands for a bit. \n\n' ...
    'You will have up to ' num2str(rest_time) ' seconds to rest. \n\n'...
    'You can press any key to end the break and continue the experiment. \n\n'];

call_expt = ['We are almost done with the experiment. \n\n' ...
    'There will just be a couple of questions for you. \n\n'...
    'Please call for the experimenter now. \n\n'];

recog_inst = ['You will now see a series of displays \n\n'...
    'that are similar to those you have just seen in the experiment. \n\n'...
    'Some of these were taken from the experiment, \n\n'...
    'while the others are completely new displays that were not shown before.'];

recog_inst_2 = ['Press the "z"  key if you think you have seen the arrangement of distractors before \n\n'...
    'Press the "/" key if you think it is a completely new display \n\n\n'...
    'Note that ALL images in this part of the experiment will be repeated \n\n'...
    'You should only press the "z" key if you think you have seen the \n\n'...
    'arrangement of distractors in the first part of the experiment \n\n\n\n'...
    'Please try to be as accurate as possible. Speed is not an issue'];

recog_inst_3 = ['You will also be asked to rate your confidence \n\n'...
    'in your decision for each display\n\n'...
    'using a scale from 1 (completely guessing) to 5 (completely confident) \n\n\n\n'...
    'Please respond using the number keys on the top of the keyboard'];

recog_inst_4 = ['Please ask the experimenter if you have any questions!'];
    


question_1 = ['Did you notice anything unusual about the search displays? \n\n'...
    'Press "Y" for yes or "N" for no'];

question_1_1 = ['Please describe what you noticed. You can use the backspace. \n\n'...
    'Please press the RETURN key when you are done.'];

question_2 = ['Some of the displays were repeated throughout the experiment. \n\n'...
    'In these displays, the distractors (orange squares) were always in the same location, \n\n'...
    'although the target could be rotated either left or right. \n\n'...
    'Did you notice? \n\n\n'...
    'Press "Y" for yes or "N" for no'];
    
% question_2 = ['During the experiment, do you think that any of the particular displays were repeated? \n\n'...
% 'Press "Y" for yes or "N" for no'];

question_3 = ['What percentage of trials do you think had repeated displays? \n\n'...
    'Please enter a number between 0 - 100. \n'...
    'Press RETURN (ENTER) when you are done. \n'...
    'You can use the backspace if you need to. \n'];

question_2_1 = ['After you realized that the displays were repeated, did you try to memorize these displays? \n\n'...
    'Press "Y" for yes or "N" for no'];


finish_message = 'The experiment has been completed. Thank you!';

abort_message = 'The experiment was manually aborted. \n\nPlease notify the experimenter.';

Screen('TextSize',window,14);
DrawFormattedText(window, inst_1, 'center', 'center', white);
Screen('Flip', window);

check_advance(startKey);


Screen('TextSize',window,14);
DrawFormattedText(window,inst_2,'center', 'center', white);

tee(window, 2, 1, Xcentre-15-150, Ycentre-10-100);
tee(window, 2, 2, Xcentre-15+150, Ycentre-10-100);


Screen('Flip',window);

% Wait until understanding confirmation

check_advance(startKey);

% while KbCheck
% end
% startexpe = [];
% 
% while isempty(startexpe)
%     [keyIsDown, KbTime, keyCode] = KbCheck;
%     if keyIsDown
%         startexpe = find(keyCode);
%         startexpe = startexpe(1);
%     end
%     if ~isempty(startexpe)
%         if startexpe(1)==startKey
%             break
%         else startexpe=[];
%         end
%         
%     end
% end

vbl=Screen('Flip',window);


Screen('TextSize',window,14);
DrawFormattedText(window, inst_3, 'center', 'center', white);
Screen('Flip', window);



check_advance(startKey);

% while KbCheck
% end
% startexpe = [];
% 
% while isempty(startexpe)
%     [keyIsDown, KbTime, keyCode] = KbCheck;
%     if keyIsDown
%         startexpe = find(keyCode);
%         startexpe = startexpe(1);
%     end
%     if ~isempty(startexpe)
%         if startexpe(1)==startKey
%             break
%         else startexpe=[];
%         end
%         
%     end
% end


Screen('TextSize',window,14);
DrawFormattedText(window, practice_inst, 'center', 'center', white);
Screen('Flip', window);




start = 0;

while start ==0 
    [down, secs, keyCode] = KbCheck;
    
    if keyCode(LeftKey) || keyCode(RightKey)
        start = 21;
    end
end
while KbCheck end

Screen('Flip', window);
WaitSecs(2);


%-------------------------------------
%     Run Practice
%-------------------------------------
% 

                
                
prac_trials = 6;
prac_table = repmat(design, prac_trials, 1); 
prac_tlocs = randsample(setdiff(1:36, vertcat(repeated_target, repeat_table.tloc, new_tlocs')), prac_trials);

for i  = 1:prac_trials
    conf_prac = sort(randsample(setdiff(1:36, prac_tlocs(i)), 5));
   
    
    for pos = conf_prac
        prac_table{i, pos+9} = 1;
    end

  

    prac_table{i, prac_tlocs(i)+9} = 0.01;
    prac_table.tid(i) = round(rand(1));
end


for trial = 1:height(prac_table)
    
%     break %% uncomment to skip
    
        fixtime=fix_time;
        iti=inter_trial+rand*0.2;
%         actual_trial = trial + (26 * (block-1));
%        
%         % update data storing variable
%         Trial(actual_trial) = actual_trial;
        
        % draw fixation
        Screen('DrawLine',window,white,Xcentre-15,Ycentre,Xcentre+15,Ycentre,2);
        Screen('DrawLine',window,white,Xcentre,Ycentre-15,Xcentre,Ycentre+15,2);
        
        % get the flip time
        vbl = Screen('Flip',window,vbl+iti-bit);
%         Fix_onset(actual_trial) = vbl;
        vbl = Screen('Flip',window,vbl+fixtime-bit);

        
        % remember, position columns are from 10-45
        for pos = 10:45

        
            if prac_table{trial,pos} == 0.01 %% target
                % grid1 converts position into x and y coordinates
                % (cx and cy)
                grid1(pos-9);
                
                % add jitter to x and y 
%                 tempx=cx+round(rand(1)*jitter)+offset;
%                 tempy=cy+(round(rand(1)*jitter))+offset;
                
                tempx = cx - jitter + (jitter - (-jitter)).*rand(1,1);
                tempy = cy - jitter + (jitter - (-jitter)).*rand(1,1);
                
                if prac_table.tid(trial)== 0
%                     Screen('DrawTexture',window, t_l, [], [tempx-15 tempy-15 tempx+15 tempy+15]); 
                    tee(window, 2, 1, tempx, tempy);
                else
%                     Screen('DrawTexture',window, t_r, [], [tempx-15 tempy-15 tempx+15 tempy+15]); 
                    tee(window, 2, 2, tempx, tempy);
                end


                
       
                
            elseif prac_table{trial,pos} == 1 % lure
                grid1(pos-9);

%                 tempx=cx+round(rand(1)*jitter)+offset;
%                 tempy=cy+(round(rand(1)*jitter))+offset;

                tempx = cx - jitter + (jitter - (-jitter)).*rand(1,1);
                tempy = cy - jitter + (jitter - (-jitter)).*rand(1,1);
                
                Screen('DrawTexture',window, l_1, [], [tempx-15 tempy-15 tempx+15 tempy+15]); 
                
            end
                  
                clear tempx
                clear tempy
            
        end
            



        vbl=Screen('Flip',window,vbl+onset_delay-bit);
        t0 = vbl;  % stimuli onset time
%         Disp_onset(actual_trial) = t0;
        % response detection
      
        flag = 0;
        while flag == 0
            [key, secs,keyCode]=KbCheck;
            if keyCode(LeftKey)||keyCode(RightKey)||keyCode(escKey)
                flag=1;
                t1=GetSecs;
            
            elseif (GetSecs-t0)> disp_time
                flag=2;  % response time out
            end
        end

        vbl = Screen('Flip',window);

        if keyCode(escKey)
            exit_flag = 1;
        end
% 
        if exit_flag
            break;
        end
         
        
        if flag == 1
%             RT(actual_trial) = (t1-t0)*1000;
            
            if keyCode(LeftKey)
                prac_resp=0;
            elseif keyCode(RightKey)
                prac_resp=1;
            end
            
            if prac_resp == prac_table.tid(trial)
%                 Error(actual_trial) = 0;
            else
%                 Error(actual_trial) = 1;
                makeBeep(750,0.25);
            end
       
            
        elseif flag == 2
            
%             RT(actual_trial) = resp_time*1000;
            makeBeep(750,0.25);
            
        end
        Screen('Flip',window);




end

WaitSecs(1);

Screen('TextSize',window,14);
DrawFormattedText(window, test_start, 'center', 'center', white);
Screen('Flip', window);




% while KbCheck
% end
% startexpe = [];
% 
% while isempty(startexpe)
%     [keyIsDown, KbTime, keyCode] = KbCheck;
%     if keyIsDown
%         startexpe = find(keyCode);
%         startexpe = startexpe(1);
%     end
%     if ~isempty(startexpe)
%         if startexpe(1)==startKey
%             break
%         else startexpe=[];
%         end
%         
%     end
% end



start = 0;

while start ==0 
    [down, secs, keyCode] = KbCheck;
    
    if keyCode(LeftKey) || keyCode(RightKey)
        start = 21;
    end
end
while KbCheck end

Screen('Flip', window);
WaitSecs(2);



%% Run Trials %%
%%%%%%%%%%%%%%%%


t_id = vertcat(zeros(13,1), ones(13,1));

% initialize a master table 
% this master table will contain ALL trials
master_table = table();


for block = 1:total_blocks
    % create a table for each block
    % combine the new_table, repeat_table, and the two targets (one repeat)
%     block_table = vertcat(new_table(new_table.block == block, :), repeat_table);
    block_table = vertcat(new_table(new_table.block == block, :), repeat_table, target_table(target_table.block == 0, :), target_table(target_table.block == block,:));
    
    % randomize trial order and target identity
    % assign to block table
    trial_order = randperm(height(block_table));
    t_id_rand = randperm(length(t_id));
    
    for trial = trial_order
       block_table.tid(trial) = t_id(t_id_rand(trial));
       block_table.block(trial) = block; 
    end

    % concatenate to master table
    master_table = [master_table; block_table];





end


        

data_table = table();
% run the actual shit


for block = 1:total_blocks
    
%     break %% uncomment to skip
    
    % get the table for the current block
    block_table = master_table(master_table.block == block, :);
    
    % randomize trial order and shuffle the block table in that order
    % also randomize target id
    trial_order_rand = randperm(height(block_table));
    block_table = block_table(trial_order_rand', :);
    t_id_rand = randperm(length(t_id));
    
    % concatenate to data table
    % this is the same as the master table, but it is randomized 
    %(master table is not randomized, thats why we need this for convenience)
    data_table = [data_table; block_table];

    for trial = 1:height(block_table)
        
        fixtime=fix_time;
        iti=inter_trial+rand*0.2;
        actual_trial = trial + (26 * (block-1));
       
        % update data storing variable
        Trial(actual_trial) = actual_trial;
        
        % draw fixation
        Screen('DrawLine',window,white,Xcentre-15,Ycentre,Xcentre+15,Ycentre,2);
        Screen('DrawLine',window,white,Xcentre,Ycentre-15,Xcentre,Ycentre+15,2);
        
        % get the flip time
        vbl = Screen('Flip',window,vbl+iti-bit);
        Fix_onset(actual_trial) = vbl;
        vbl = Screen('Flip',window,vbl+fixtime-bit);

        
        % remember, position columns are from 10-45
        for pos = 10:45
            
            if numel(num2str((block_table{trial, pos}))) == 8
                    layout = (block_table{trial, pos} - mod(block_table{trial, pos}, 10))/10;
            else
                    layout = block_table{trial, pos};
            end

        
            if layout <0 %% target
                % grid1 converts position into x and y coordinates
                % (cx and cy)
                grid1(pos-9);
                
                % add jitter to x and y 
%                 tempx=cx+round(rand(1)*jitter)+offset;
%                 tempy=cy+(round(rand(1)*jitter))+offset;

                xjitter = (mod(-layout, 1000000) - mod(-layout, 1000))/ 1000;
                yjitter = (mod(-layout, 1000) - mod(-layout, 10));
                
                if mod(xjitter, 10) == 1
                    xjitter = (xjitter - mod(xjitter, 10))/10;
                else
                    xjitter = -(xjitter - mod(xjitter, 10))/10;
                end
                
                if mod(yjitter, 10) == 1
                    yjitter = (yjitter - mod(yjitter, 10))/10;
                else
                    yjitter = -(yjitter - mod(yjitter, 10))/10;
                end
                

                tempx = cx + xjitter;
                tempy = cy + yjitter;
                
                if block_table.tid(trial) == 0
%                     Screen('DrawTexture',window, t_l, [], [tempx-15 tempy-15 tempx+15 tempy+15]);
                        tee(window, 2, 1, tempx, tempy);
                else
%                     Screen('DrawTexture',window, t_r, [], [tempx-15 tempy-15 tempx+15 tempy+15]);
                        tee(window, 2, 2, tempx, tempy);
                end
                
     
            elseif block_table{trial,pos} >= 1 % distractor
                grid1(pos-9);

%                 tempx=cx+round(rand(1)*jitter)+offset;
% %                 tempy=cy+(round(rand(1)*jitter))+offset;
%                 tempx = cx - jitter + (jitter - (-jitter)).*rand(1,1);
%                 tempy = cy - jitter + (jitter - (-jitter)).*rand(1,1);
                

                tempor = (layout - mod(layout, 1000000))/1000000;
                xjitter = (mod(layout, 1000000) - mod(layout, 1000))/ 1000;
                yjitter = (mod(layout, 1000) - mod(layout, 10));
                
                if mod(xjitter, 10) == 1
                    xjitter = (xjitter - mod(xjitter, 10))/10;
                else
                    xjitter = -(xjitter - mod(xjitter, 10))/10;
                end
                
                if mod(yjitter, 10) == 1
                    yjitter = (yjitter - mod(yjitter, 10))/10;
                else
                    yjitter = -(yjitter - mod(yjitter, 10))/10;
                end
                                
                
                
                tempx = cx + xjitter;
                tempy = cy + yjitter;
 
%                 if numel(num2str((block_table{trial, pos}))) == 7
%                     ell(window, 2, tempor, tempx, tempy);
%                 else
                    Screen('DrawTexture',window, l_1, [], [tempx-15 tempy-15 tempx+15 tempy+15]); 
%                 end
                
            end
                  
                clear tempx
                clear tempy
            
        end
            



        vbl=Screen('Flip',window,vbl+onset_delay-bit);
        t0 = vbl;  % stimuli onset time
        Disp_onset(actual_trial) = t0;
        
        
        im=Screen('GetImage',window, rect); %% Gavin--here is the start of the relevant code
        f=[pwd '\' int2str(subject_id) '\' int2str(actual_trial) '.png'];
        imwrite(im,f,'PNG'); % JPEG saved... not sure if JPEG is the best for you or not--you could probably save as a .gif or something else that might keep the fidelity high
                
        % response detection
        flag = 0;
        while flag == 0
            [key, secs,keyCode]=KbCheck;
            if keyCode(LeftKey)||keyCode(RightKey)||keyCode(escKey)
                flag=1;
                t1=GetSecs;
            
            elseif (GetSecs-t0)> disp_time
                flag=2;  % response time out
            end
        end

        vbl = Screen('Flip',window);

        if keyCode(escKey)
            exit_flag = 1;
        end
% 
        if exit_flag
            break;
        end
         
        
        if flag == 1
            RT(actual_trial) = (t1-t0)*1000;
            
            if keyCode(LeftKey)
                Resp(actual_trial)=0;
            elseif keyCode(RightKey)
                Resp(actual_trial)=1;
            end
            
            if Resp(actual_trial) == block_table.tid(trial)
                Error(actual_trial) = 0;
            else
                Error(actual_trial) = 1;
                makeBeep(750,0.25);
            end
       
            
        elseif flag == 2
            
            RT(actual_trial) = resp_time*1000;
            makeBeep(750,0.25);
            
        end
        Screen('Flip',window);




    end

    if exit_flag
        break;
    end    
    
     % end of block
    if block~=total_blocks  % not the last block, do rest period
        DrawFormattedText(window,rest_inst,'center','center',[255 255 255]);
        Screen('Flip',window);
        
        WaitSecs(1);
        tt=GetSecs;
        flag=0;
        while (flag==0) && (GetSecs-tt)<rest_time
            [key,secs,keyCode]=KbCheck;
            if key
                flag=1;
            end
        end
        
        Screen('Flip',window);
        
        WaitSecs(2);
        vbl=Screen('Flip',window);
        
    end
            
            

    
end
    
    

    
    
%% END OF EXPT %%
%%%%%%%%%%%%%%%%%
% 


sub_id = repmat(subject_id, size(RT));


% pad the data_table if experiment was aborted 
data_table = vertcat(data_table, repmat(design, total_trials - height(data_table), 1));
data_table_out = [table(sub_id, Trial, RT, Resp, Error, Fix_onset, Disp_onset) data_table];


data_table_out = sortrows(data_table_out, 2);


filename = ['CC_', num2str(subject_id), '.csv'];
writetable(data_table_out, filename, 'Delimiter', ',');

save(['CC_', num2str(subject_id), '.mat']);





    
if exit_flag
    % the experiment was aborted
    Screen('Flip',window);
    DrawFormattedText(window,abort_message,'center','center',white, 100, 0, 0, 2);
    Screen('Flip',window);
    question1_resp = -1;
    keybuffer = -1;
    
    
    check_advance(startKey);
    
%     while KbCheck
%     end
%     startexpe = [];
% 
%     while isempty(startexpe)
%         [keyIsDown, KbTime, keyCode] = KbCheck;
%         if keyIsDown
%             startexpe = find(keyCode);
%             startexpe = startexpe(1);
%         end
%         if ~isempty(startexpe)
%             if startexpe(1)==startKey
%                 break
%             else startexpe=[];
%             end
% 
%         end
%     end
%     
    
else

    
    
    %%%%% RECOGNITION TEST %%%%%
    
    
    % end of search task
    
    
    
    
    % call experimenter 
    Screen('TextFont',window,'Gilsans');
    Screen('TextSize',window,15);
    DrawFormattedText(window,call_expt,'center','center',white);
    Screen('Flip',window);
    
    check_advance(startKey);
    
    % question 1: notice anything unusual?
    
    Screen('TextFont',window,'Gilsans');
    Screen('TextSize',window,15);
    DrawFormattedText(window,question_1,'center', 'center', white, 100, 0, 0, 2);
    Screen('Flip',window);
    
    
    [~, keyCode] = KbWait; 
    while ~ismember(find(keyCode),[78,89])
        [~, keyCode] = KbWait(0,2);
    end
    if find(keyCode) == 89
        question1_resp = 1;
    elseif find(keyCode) == 78
        question1_resp = 0;
    end
    
    if question1_resp == 1
        % question 1_1: if answered yes to the above -- describe it 
        DrawFormattedText(window, question_1_1, 'center', Ycentre-200, white, 100, 0, 0, 2);
        Screen('Flip', window);
        
        
%         q1_1_resp =Ask(window,question_1_1,[127],[0],'GetChar',RectLeft,RectTop); % Accept keyboard input, echo it to screen.

        KbQueueCreate;
        KbQueueStart;

        enterpressed = 0;
        keybuffer = [];

        while enterpressed == 0

            % check for keypress and whether enter is pressed or not
            [pressed, keypress] = KbQueueCheck; 
            enterpressed = keypress(returnKey);

            if pressed && ~enterpressed
                if keypress(deleteKey)
                    keybuffer = keybuffer(1:end-1);
                else
                    % remove zeros
                    keypress(find(keypress==0))=NaN;
                    % get the RT and index of keypress
                    [endtime index] = min(keypress);
                    
                    if KbName(index) == 'space'
                        keybuffer = [keybuffer ' '];
                    else 
                        keybuffer = [keybuffer KbName(index)];
                    end
                end

                % display keys pressed
                DrawFormattedText(window, question_1_1, 'center', Ycentre-200, white, 100, 0, 0, 2);
                DrawFormattedText(window, keybuffer, 'center', Ycentre+50);
                Screen('Flip', window);


            end
            WaitSecs(.01);
            q1_1_resp = keybuffer;
        end
        
    else
        q1_1_resp = '99999';
    end
    
    % question 2 - something funky this way comes
    DrawFormattedText(window, question_2, 'center', Ycentre-200, white, 100, 0, 0, 2);
    Screen('Flip', window);
    
    [~, keyCode] = KbWait; 
    while ~ismember(find(keyCode),[78,89])
        [~, keyCode] = KbWait(0,2);
    end
    if find(keyCode) == 89
        question2_resp = 1;
    elseif find(keyCode) == 78
        question2_resp = 0;
    end

    % question 3 - proportion of trials
    
    DrawFormattedText(window, question_3, 'center', Ycentre-200, white, 100, 0, 0, 2);
    Screen('Flip', window);
    
    
    KbQueueCreate;
    KbQueueStart;
    
    enterpressed = 0;
    keybuffer = [];
    
    while enterpressed == 0
        
        % check for keypress and whether enter is pressed or not
        [pressed, keypress] = KbQueueCheck;
        enterpressed = keypress(returnKey);
        
        if pressed && ~enterpressed
            if keypress(deleteKey)
                keybuffer = keybuffer(1:end-1);
            else
                % remove zeros
                keypress(find(keypress==0))=NaN;
                % get the RT and index of keypress
                [endtime index] = min(keypress);
                % not sure why but it gives me the wrong KbName (e.g. 8
                % becomes 8*, 5 becomes 5% etc. so plus 48
                keybuffer = [keybuffer KbName(index+48)];
            end
            
            % display keys pressed
            DrawFormattedText(window, question_3, 'center', Ycentre-200, white, 100, 0, 0, 2);
            DrawFormattedText(window, keybuffer, 'center', Ycentre+50);
            Screen('Flip', window);
            
            
        end
        WaitSecs(.01);
    end
    
    
    
    % recognition test instructions 
    DrawFormattedText(window, recog_inst, 'center', 'center');
    Screen('Flip', window);    
    check_advance(startKey);
    
    DrawFormattedText(window, recog_inst_2, 'center', 'center');
    Screen('Flip', window);
    check_advance(startKey);
    
    DrawFormattedText(window, recog_inst_3, 'center', 'center');
    Screen('Flip', window);
    check_advance(startKey);
    
    DrawFormattedText(window, recog_inst_4, 'center', 'center');
    Screen('Flip', window);
    check_advance(startKey);
    
    
    %% Start recognition test
    
    for trial = 1:height(recognition_table)
        
        fixtime=fix_time;
        iti=inter_trial+rand*0.2;
%         actual_trial = trial + (26 * (block-1));
       
        % update data storing variable
        Trial_recog(trial) = trial;
        
%         % draw fixation
%         Screen('DrawLine',window,white,Xcentre-15,Ycentre,Xcentre+15,Ycentre,2);
%         Screen('DrawLine',window,white,Xcentre,Ycentre-15,Xcentre,Ycentre+15,2);
        
        % get the flip time
%         vbl = Screen('Flip',window,vbl+iti-bit);
%         Fix_onset(trial) = vbl;
        vbl = Screen('Flip',window,vbl+fixtime-bit);

        
        % remember, position columns are from 10-45
        for pos = 10:45
            
            if numel(num2str((recognition_table{trial, pos}))) == 8
                    layout = (recognition_table{trial, pos} - mod(recognition_table{trial, pos}, 10))/10;
            else
                    layout = recognition_table{trial, pos};
            end

        
            if layout <0 %% target
                % grid1 converts position into x and y coordinates
                % (cx and cy)
                grid1(pos-9);
                
                % add jitter to x and y 
%                 tempx=cx+round(rand(1)*jitter)+offset;
%                 tempy=cy+(round(rand(1)*jitter))+offset;

                xjitter = (mod(-layout, 1000000) - mod(-layout, 1000))/ 1000;
                yjitter = (mod(-layout, 1000) - mod(-layout, 10));
                
                if mod(xjitter, 10) == 1
                    xjitter = (xjitter - mod(xjitter, 10))/10;
                else
                    xjitter = -(xjitter - mod(xjitter, 10))/10;
                end
                
                if mod(yjitter, 10) == 1
                    yjitter = (yjitter - mod(yjitter, 10))/10;
                else
                    yjitter = -(yjitter - mod(yjitter, 10))/10;
                end
                

                tempx = cx + xjitter;
                tempy = cy + yjitter;
                
                if recognition_table.tid(trial) == 0
%                     Screen('DrawTexture',window, t_l, [], [tempx-15 tempy-15 tempx+15 tempy+15]);
                        tee(window, 2, 1, tempx, tempy);
                else
%                     Screen('DrawTexture',window, t_r, [], [tempx-15 tempy-15 tempx+15 tempy+15]);
                        tee(window, 2, 2, tempx, tempy);
                end
                
     
            elseif recognition_table{trial,pos} >= 1 % distractor
                grid1(pos-9);

%                 tempx=cx+round(rand(1)*jitter)+offset;
% %                 tempy=cy+(round(rand(1)*jitter))+offset;
%                 tempx = cx - jitter + (jitter - (-jitter)).*rand(1,1);
%                 tempy = cy - jitter + (jitter - (-jitter)).*rand(1,1);
                

                tempor = (layout - mod(layout, 1000000))/1000000;
                xjitter = (mod(layout, 1000000) - mod(layout, 1000))/ 1000;
                yjitter = (mod(layout, 1000) - mod(layout, 10));
                
                if mod(xjitter, 10) == 1
                    xjitter = (xjitter - mod(xjitter, 10))/10;
                else
                    xjitter = -(xjitter - mod(xjitter, 10))/10;
                end
                
                if mod(yjitter, 10) == 1
                    yjitter = (yjitter - mod(yjitter, 10))/10;
                else
                    yjitter = -(yjitter - mod(yjitter, 10))/10;
                end
                                
                
                
                tempx = cx + xjitter;
                tempy = cy + yjitter;
 
%                 if numel(num2str((block_table{trial, pos}))) == 7
%                     ell(window, 2, tempor, tempx, tempy);
%                 else
                    Screen('DrawTexture',window, l_1, [], [tempx-15 tempy-15 tempx+15 tempy+15]); 
%                 end
                
            end
                  
                clear tempx
                clear tempy
            
        end
            



        vbl=Screen('Flip',window,vbl+onset_delay-bit);
        t0 = vbl;  % stimuli onset time
%         Disp_onset(trial) = t0;
        
        
        im=Screen('GetImage',window, rect); %% Gavin--here is the start of the relevant code
        f=[pwd '\' int2str(subject_id) '\' int2str(trial) '_recognition.png'];
        imwrite(im,f,'PNG'); % JPEG saved... not sure if JPEG is the best for you or not--you could probably save as a .gif or something else that might keep the fidelity high
                
        % response detection
        flag = 0;
        while flag == 0
            [key, secs,keyCode]=KbCheck;
%             if keyCode(LeftKey)||keyCode(RightKey)||keyCode(escKey)
            if keyCode(escKey) || keyCode(KbName('z')) || keyCode(KbName('/?'))
                flag=1;
                t1=GetSecs;
            
            elseif (GetSecs-t0)> recognition_time
                flag=2;  % response time out
            end
        end

        vbl = Screen('Flip',window);

        if keyCode(escKey)
            exit_flag = 1;
        end
% 
        if exit_flag
            break;
        end
         
        
        if flag == 1
            RT_recog(trial) = (t1-t0)*1000;
            
            if keyCode(KbName('z'))
                Resp_recog(trial)=1;
            elseif keyCode(KbName('/?'))
                Resp_recog(trial)=0;
            end
            
            if Resp_recog(trial) == recognition_table.repeat(trial)
                Hit_recog(trial) = 1;
            else
                Hit_recog(trial) = 0;
%                 makeBeep(750,0.25);
            end
       
            
        elseif flag == 2
            
            RT_recog(trial) = resp_time*1000;
%             makeBeep(750,0.25);
            
        end
        Screen('Flip',window);

        %% COnfidence rating screen
        
      
        
        Screen('FrameRect', window, [127], [Xcentre-400-50 Ycentre-50 Xcentre-400+50 Ycentre+50]);
        Screen('FrameRect', window, [127], [Xcentre-200-50 Ycentre-50 Xcentre-200+50 Ycentre+50]);
        Screen('FrameRect', window, [127], [Xcentre-50 Ycentre-50 Xcentre+50 Ycentre+50]);
        Screen('FrameRect', window, [127], [Xcentre+200-50 Ycentre-50 Xcentre+200+50 Ycentre+50]);
        Screen('FrameRect', window, [127], [Xcentre+400-50 Ycentre-50 Xcentre+400+50 Ycentre+50]);
        
        
        
        Screen('TextSize',window,20);
        DrawFormattedText(window, num2str(1), Xcentre-400-10, Ycentre-20, [127]);
        DrawFormattedText(window, num2str(2), Xcentre-200-10, Ycentre-20, [127]);
        DrawFormattedText(window, num2str(3), Xcentre-10, Ycentre-20, [127]);
        DrawFormattedText(window, num2str(4), Xcentre+200-10, Ycentre-20, [127]);
        DrawFormattedText(window, num2str(5), Xcentre+400-10, Ycentre-20, [127]);
        
        DrawFormattedText(window, 'Complete\nguessing', Xcentre-450, Ycentre-150, [127]);
        DrawFormattedText(window, 'Complete\nconfidence', Xcentre+350, Ycentre-150, [127]);
        
        
        Screen('Flip',window)
        
        
        [~, keyCode] = KbWait; 
        while ~ismember(find(keyCode),[49, 50, 51, 52, 53])
            [~, keyCode] = KbWait(0,2);
        end
        
        if find(keyCode) == 49
           Confidence_recog(trial) = 1;
           
        elseif find(keyCode) == 50
            Confidence_recog(trial) = 2;
            
        elseif find(keyCode) == 51
            Confidence_recog(trial) = 3;
            
        elseif find(keyCode) == 52
            Confidence_recog(trial) = 4;
            
        elseif find(keyCode) == 53
            Confidence_recog(trial) = 5;
            
            
        end

        vbl = Screen('Flip',window);

        if keyCode(escKey)
            exit_flag = 1;
        end
% 
        if exit_flag
            break;
        end
         
        



    end


    % display end message
    DrawFormattedText(window, finish_message, 'center', 'center', [255 255 255]);
    Screen('Flip', window);
   


    

    
end

%% SAVE DATA %%
%%%%%%%%%%%%%%%


sub_id = repmat(subject_id, size(RT));
% q2 = repmat(keybuffer, size(RT));

% pad the data_table if experiment was aborted 
data_table = vertcat(data_table, repmat(design, total_trials - height(data_table), 1));
data_table_out = [table(sub_id, Trial, RT, Resp, Error, Fix_onset, Disp_onset) data_table];



q1 = repmat(question1_resp, size(RT_recog));
q1_1 = repmat(q1_1_resp, size(RT_recog));
q2 = repmat(keybuffer, size(RT_recog));

sub_id_2 = repmat(subject_id, size(RT_recog));
data_table_recog = [table(sub_id_2, Trial_recog, RT_recog, Resp_recog, Hit_recog, Confidence_recog, q1, q1_1, q2) recognition_table];

data_table_out = sortrows(data_table_out, 2);


filename = ['CC_', num2str(subject_id), '.csv'];
writetable(data_table_out, filename, 'Delimiter', ',');

filename_2 = ['CC_', num2str(subject_id), '_recognition.csv'];
writetable(data_table_recog, filename_2, 'Delimiter', ',');

save(['CC_', num2str(subject_id), '.mat']);

% end timer
expt_time = toc/60;

 while KbCheck
    end
    startexpe = [];

    while isempty(startexpe)
        [keyIsDown, KbTime, keyCode] = KbCheck;
        if keyIsDown
            startexpe = find(keyCode);
            startexpe = startexpe(1);
        end
        if ~isempty(startexpe)
            if startexpe(1)==startKey
                break
            else startexpe=[];
            end

        end


    end
    
sca;
return;









