function ell(tp,col,or,x,y); 
    %tp=texture index:which image to draw 'L' on
    %col=color
    %or=orientation of distractor
    %x and y are coordinates for top-left corner of letter
    
    lgth=20;
    xpos=x;
    ypos=y;
    width=3;

    if(col==1) % blue 
        colvalue=[0, 0, 255];
    elseif(col==2) % red
        colvalue=[255, 0, 0];
    end;

switch or
    case 1 % up left
Screen('DrawLine',tp,colvalue,xpos+lgth,ypos,xpos+lgth,ypos+lgth,width);
Screen('DrawLine',tp,colvalue,xpos,ypos+lgth,xpos+lgth,ypos+lgth,width);
    case 2 % up right    
Screen('DrawLine',tp,colvalue,xpos,ypos,xpos,ypos+lgth,width);
Screen('DrawLine',tp,colvalue,xpos,ypos+lgth,xpos+lgth,ypos+lgth,width);
    case 3 % down right
Screen('DrawLine',tp,colvalue,xpos,ypos,xpos,ypos+lgth,width);
Screen('DrawLine',tp,colvalue,xpos,ypos,xpos+lgth,ypos,width);        
    case 4 % down left
Screen('DrawLine',tp,colvalue,xpos+lgth,ypos,xpos+lgth,ypos+lgth,width);
Screen('DrawLine',tp,colvalue,xpos,ypos,xpos+lgth,ypos,width);        
end;
    
    
    