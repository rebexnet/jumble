namespace Jumble

open System
open System.Runtime.InteropServices
open Microsoft.Win32.SafeHandles
open Serilog
open System.IO

// https://stackoverflow.com/questions/410705/best-way-to-determine-if-two-path-reference-to-same-file-in-c-sharp

[<RequireQualifiedAccess>]
module FilePathComparer =
    type private FILETIME = UInt64

    [<StructLayout(LayoutKind.Sequential, Pack = 4)>]    
    type private BY_HANDLE_FILE_INFORMATION =
        struct 
            val mutable FileAttributes:UInt32
            val mutable CreationTime:FILETIME
            val mutable LastAccessTime:FILETIME
            val mutable LastWriteTime:FILETIME
            val mutable VolumeSerialNumber:UInt32
            val mutable FileSizeHigh:UInt32
            val mutable FileSizeLow:UInt32
            val mutable NumberOfLinks:UInt32
            val mutable FileIndexHigh:UInt32
            val mutable FileIndexLow:UInt32   
        end

    type FileInformation = {
        VolumeSerialNumber: uint32
        FileIndexHigh: uint32
        FileIndexLow: uint32
    }
    
    [<AbstractClass; Sealed>]
    type private NativeMethods() = 
        [<DllImport("kernel32.dll", SetLastError = true)>]
        static extern bool GetFileInformationByHandle(SafeFileHandle hFile, BY_HANDLE_FILE_INFORMATION& lpFileInformation);
    
        [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        static extern SafeFileHandle CreateFile([<MarshalAs(UnmanagedType.LPTStr)>] string filename,
                                                 [<MarshalAs(UnmanagedType.U4)>] FileAccess access,
                                                 [<MarshalAs(UnmanagedType.U4)>] FileShare share,
                                                 IntPtr securityAttributes,
                                                 [<MarshalAs(UnmanagedType.U4)>] FileMode creationDisposition,
                                                 [<MarshalAs(UnmanagedType.U4)>] FileAttributes flagsAndAttributes,
                                                 IntPtr templateFile);

        static member GetFileInformation path =
            try 
                use sfh : SafeFileHandle = CreateFile(path, FileAccess.Read, FileShare.ReadWrite, IntPtr.Zero, FileMode.Open, enum 0, IntPtr.Zero)
                if sfh.IsInvalid then Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error())

                let mutable fi = Unchecked.defaultof<BY_HANDLE_FILE_INFORMATION>
                if GetFileInformationByHandle(sfh, &fi) = false then raise (IOException(sprintf "GetFileInformationByHandle has failed on path"));
                fi
            with
            | :? UnauthorizedAccessException ->
                Log.Error("Cannot access file {File:l}", path)
                reraise()

    let getFileInformation path = 
        let bhfi = NativeMethods.GetFileInformation path
        { VolumeSerialNumber = bhfi.VolumeSerialNumber; FileIndexHigh = bhfi.FileIndexHigh; FileIndexLow = bhfi.FileIndexLow }