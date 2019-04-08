function grid1(sp)
    global cx;
    global cy;
    global Xcentre;
    global Ycentre;
    

% 12 x 12 display, 144 possible locations
% get the grid sizes of X and Y
% X = 1024/9;
% Y = 768/9;

pad = 10;
jitter = 20;
% 
% % starting coordinates of grid
% coords_x = [round(X * (0:8))];
% coords_y = [round(Y * (0:8))];

coords_x = linspace(0,1024,8);
coords_y = linspace(0,768,8);



y = ceil(sp/6);
x = mod(sp, 6);
if x == 0
    x = 6;
end
% cx = coords_x(x+1) -jitter + (jitter-(-jitter)).*rand(1,1) ;
% cy = coords_y(y+1) -jitter + (jitter-(-jitter)).*rand(1,1) ;

cx = coords_x(x+1);
cy = coords_y(y+1);


end
% 
% for i = l
%     x = ceil(i/12);
%     y = mod(i,12);
%     if y == 0
%         y = 3;
%     end
%     
%     cx = coords_x(x);
%     cy = coords_y(y);
% end



