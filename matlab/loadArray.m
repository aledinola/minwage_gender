function x = loadArray(filename,siz)
% LOADARRAY Load and reshape a flattened Fortran text array.
%
% INPUTS:
% filename: Text-file path
% siz:      [1,n_dims] target dimensions
%
% OUTPUTS:
% x:        Array with dimensions siz

values = dlmread(filename,'\t');
values = values(:);
if numel(values) ~= prod(siz)
    error('loadArray:WrongSize','%s has %d values; expected %d.', ...
        filename,numel(values),prod(siz));
end
x = reshape(values,siz);
end %end function
