function copyFigurePair(sourceDir,sourceName,outputDir,outputName)
% COPYFIGUREPAIR Copy a generated EPS/PNG pair to its paper number.
%
% INPUTS:
% sourceDir:  Directory containing the generated pair
% sourceName: Generated filename without extension
% outputDir:  Final output-directory path
% outputName: Numbered filename without extension
%
% OUTPUTS:
% None

copyfile(fullfile(sourceDir,[sourceName '.eps']),fullfile(outputDir,[outputName '.eps']));
copyfile(fullfile(sourceDir,[sourceName '.png']),fullfile(outputDir,[outputName '.png']));
end %end function
