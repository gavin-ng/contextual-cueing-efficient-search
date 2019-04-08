function check_advance(key)

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
            if startexpe(1)==key
                break
            else startexpe=[];
            end

        end


    end