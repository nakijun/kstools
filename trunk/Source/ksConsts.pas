unit ksConsts;

interface

resourcestring
  SInvalidArchive = 'Invalid archive "%s"';
  SInvalidFileSignature = 'Invalid file "%s" signature ($0%x)';
  SEndOfCentralDirNotFound = 'End of central directory record not found for "%s"';
  SArchiveIsReadOnly = 'Archive "%s" is read only';
  SInvalidPassword = 'Invalid password';
  SInvalidMethod = 'Compression method %d not supported';
  SInvalidCRC = 'CRC Error for file %d "%s"';
  SInvalidSize = 'File Size Error for file %d "%s" - expected %d, found %d';

implementation

end.
