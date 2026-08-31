function varargout = unpack(S,varargin)
% UNPACK Return selected structure fields as separate outputs.
%
% INPUTS:
% S:        Input structure
% varargin: Field names
%
% OUTPUTS:
% varargout: Values of the requested fields
%
% unpack extracts selected fields of S and puts them in new variables
% EXAMPLE
% par.a = 3.5;
% par.b = [4;1.2];
% par.c = 'ale';
% [b,c] = unpack(par,'b','c') % no cell array

varargout = varargin; % simpler preallocation :)
for k = 1:numel(varargin)
    varargout{k} = S.(varargin{k});
end
end %end function
