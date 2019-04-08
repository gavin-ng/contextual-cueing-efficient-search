function tee(tp,col,or,x,y); 
    %tp=texture index:which image to draw 'T' on
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
    
    switch or % right
        case 1
            Screen('DrawLine',tp,colvalue,xpos,ypos,xpos,ypos+lgth,width);
            Screen('DrawLine',tp,colvalue,xpos,ypos+(lgth/2),xpos+lgth,ypos+(lgth/2),width);
        case 2 % left
            Screen('DrawLine',tp,colvalue,xpos+lgth,ypos,xpos+lgth,ypos+lgth,width);
            Screen('DrawLine',tp,colvalue,xpos,ypos+(lgth/2),xpos+lgth,ypos+(lgth/2),width);
        case 3 % down
            Screen('DrawLine',tp,colvalue,xpos+(lgth/2),ypos,xpos+(lgth/2),ypos+lgth,width);
            Screen('DrawLine',tp,colvalue,xpos,ypos,xpos+lgth,ypos,width);
        case 4 % up
            Screen('DrawLine',tp,colvalue,xpos+(lgth/2),ypos,xpos+(lgth/2),ypos+lgth,width);
            Screen('DrawLine',tp,colvalue,xpos,ypos+lgth,xpos+lgth,ypos+lgth,width);
    end;
    
    