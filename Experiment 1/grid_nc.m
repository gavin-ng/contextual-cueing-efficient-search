function [C, whichring] = grid_nc( sp )
%GRID_NC Summary of this function goes here
%   Detailed explanation goes here

global cx;
global cy;
global Xcentre;
global Ycentre;

% basic radius parameter
r1=105;  % for 1024*768
dr1=3;

switch ceil(sp/9)  % deciding which quadrant this point falls into
    case 1  % left top quadrant, sp=1~9
        
        switch mod(sp,3)
            case 1 % sp=1, 4, 7
                theta=pi/12; % angle of axis
                B=13/7; % multiplier for further layers
                C = 0.32;
            case 2 % sp=2, 5, 8
                theta=3*pi/12; % 45 degree axis
                B=13/7; % multiplier is smaller for axes that are more vertical
                C = 0.36;
            case 0 % sp=3, 6, 9
                theta=5*pi/12; % most vertical axis
                B=13/7; % smallest multiplier value
                C=0.40;
        end
        % deciding the 'layer', which will specify the basic radius and the
        % radius of jitter
        if sp<=3  % inner layer
            r=r1;
            dr=dr1; % radius of jitter
%             if sp~=2 % positions 1, 3 are moved inward to make another more inner layer
%                 r=r1/2;
%                 dr=7;
%             end
            whichring=1;
        elseif sp<=6  % middle layer
            r=B*r1;
            dr=dr1*B;
            whichring=2;
        else  % outer layer
            r=B^2*r1;
            dr=dr1*B^2;
            whichring=3;
        end
    case 2  % right top quadrant
        switch mod(sp,3)
            case 1
                theta=7*pi/12;
                B=13/7;
                C = 0.40;
            case 2
                theta=9*pi/12;
                B=13/7;
                C = 0.36;
            case 0
                theta=11*pi/12;
                B=13/7;
                C=0.32;
        end
        if sp<=12
            r=r1;
            dr=10;
            whichring=1;
%             if sp==11; % for this quadrant only one is pushed inward
%                 r=r1/2;
%                 dr=7;
%             end
        elseif sp<=15
            r=B*r1;
            dr=dr1*B;
            whichring=2;
        else
            r=B^2*r1;
            dr=dr1*B^2;
            whichring=3;
        end
    case 3  % left bottom quadrant
        switch mod(sp,3)
            case 1
                theta=19*pi/12;
                B=13/7;
                C=0.40;
            case 2
                theta=21*pi/12;
                B=13/7;
                C=0.36;
            case 0
                theta=23*pi/12;
                B=13/7;
                C=0.32;
        end
        if sp<=21
            r=r1;
            dr=10;
            whichring=1;
%             if sp==20
%                 r=r1/2;
%                 dr=7;
%             end
        elseif sp<=24
            r=B*r1;
            dr=dr1*B;
            whichring=2;
        else
            r=B^2*r1;
            dr=dr1*B^2;
            whichring=3;
        end
    case 4  % right bottom quadrant
        switch mod(sp,3)
            case 1
                theta=13*pi/12;
                B=13/7;
                C=0.32;
            case 2
                theta=15*pi/12;
                B=13/7;
                C=0.36;
            case 0
                theta=17*pi/12;
                B=13/7;
                C=0.40;
        end
        if sp<=30
            r=r1;
            dr=10;
            whichring=1;
%             if sp~=29
%                 r=r1/2;
%                 dr=7;
%             end
        elseif sp<=33
            r=B*r1;
            dr=dr1*B;
            whichring=2;
        else
            r=B^2*r1;
            dr=dr1*B^2;
            whichring=3;
        end
end

% each possible position's x & y coordinates are easily transformed from
% radius-angle coordinates
cx=round(Xcentre-r*cos(theta));
cy=round(Ycentre-r*sin(theta));


% add random jitter according to location
% a simple 'uniformly distributed' (probably not really uniform) circular jitter
% whose size is determined by 'dr'
delta_r=dr*rand(1);
phi=2*pi*rand(1);
cx=round(cx+delta_r*cos(phi));
cy=round(cy+delta_r*sin(phi));
