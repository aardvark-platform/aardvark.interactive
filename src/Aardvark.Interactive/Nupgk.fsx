#r "System.IO.Compression.dll"

open System
open System.Text
open System.IO
open System.IO.Compression
open System.Security.Cryptography

[<RequireQualifiedAccess>]
type License =
    | Url of string
    | Expression of string

type NupkgProperties =
    {
        name            : string
        version         : string
        description     : string
        authors         : string
        license         : License
        url             : string
        releaseNotes    : string
        dependencies    : list<string * string>
        files           : list<string * string>
    }

module Nupkg =
    
    type ZipArchiveEntry with
        member x.Write (data : string) =
            use s = x.Open()
            use w = new StreamWriter(s, Encoding.UTF8)
            w.Write(data)
            w.Flush()
        member x.Write (lines : #seq<string>) =
            use s = x.Open()
            use w = new StreamWriter(s, Encoding.UTF8)
            w.Write(String.concat "\r\n" lines)
            w.Flush()

    type Stream with
        member x.ReadToEnd() =
            let mutable buffer = Array.zeroCreate 65536
            let mutable o = 0
            let mutable cont = true
            while cont do
                if o >= buffer.Length then Array.Resize(&buffer, 2*buffer.Length)

                let rem = buffer.Length - o
                let v = x.Read(buffer, o, rem)
                if v <= 0 then cont <- false
                o <- o + v

            if buffer.Length > o then Array.Resize(&buffer, o)
            buffer

    let create (props : NupkgProperties) =
        use ms = new MemoryStream()
        use arch = new ZipArchive(ms, ZipArchiveMode.Create, true, Encoding.UTF8)

        let e = arch.CreateEntry("_rels/.rels")
        e.Write [
            sprintf "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>"
            sprintf "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
            sprintf "  <Relationship Type=\"http://schemas.microsoft.com/packaging/2010/07/manifest\" Target=\"/%s.nuspec\" Id=\"nuspec\" />" props.name
            sprintf "  <Relationship Type=\"http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties\" Target=\"/package/services/metadata/core-properties/coreProp.psmdcp\" Id=\"coreProp\" />"
            sprintf "</Relationships>"
        ]


        let e = arch.CreateEntry "package/services/metadata/core-properties/coreProp.psmdcp"
        e.Write [
            sprintf "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>"
            sprintf "<coreProperties xmlns=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:dcterms=\"http://purl.org/dc/terms/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
            sprintf "  <dc:creator>%s</dc:creator>" props.authors
            sprintf "  <dc:description>%s</dc:description>" props.description
            sprintf "  <dc:identifier>%s</dc:identifier>" props.name
            sprintf "  <version>%s</version>" props.version
            sprintf "  <keywords />"
            sprintf "  <dc:title>%s</dc:title>" props.name
            sprintf "  <lastModifiedBy>nupkg</lastModifiedBy>"
            sprintf "</coreProperties>"
        ]

        let e = arch.CreateEntry (sprintf "%s.nuspec" props.name)
        e.Write [
            sprintf "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>"
            sprintf "<package xmlns=\"http://schemas.microsoft.com/packaging/2011/10/nuspec.xsd\">"
            sprintf "  <metadata>"
            sprintf "    <id>%s</id>" props.name
            sprintf "    <version>%s</version>" props.version
            sprintf "    <title>%s</title>" props.name
            sprintf "    <authors>%s</authors>" props.authors
            sprintf "    <owners>%s</owners>" props.authors
            match props.license with
            | License.Url u -> sprintf "    <licenseUrl>%s</licenseUrl>" u
            | License.Expression e -> sprintf "    <licenseExpression>%s</licenseExpression>" e
            sprintf "    <projectUrl>%s</projectUrl>" props.url
            sprintf "    <description>%s</description>" props.description
            sprintf "    <releaseNotes>%s</releaseNotes>" props.releaseNotes
            sprintf "    <dependencies>"
            for (name, version) in props.dependencies do
                sprintf "      <dependency id=\"%s\" version=\"%s\" />" name version
            sprintf "    </dependencies>"
            sprintf "  </metadata>"
            sprintf "</package>"
        ]


        let e = arch.CreateEntry "[Content_Types].xml"
        e.Write [
            "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>"
            "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
            "  <Default Extension=\"xml\" ContentType=\"application/octet\" />"
            "  <Default Extension=\"pdb\" ContentType=\"application/octet\" />"
            "  <Default Extension=\"dll\" ContentType=\"application/octet\" />"
            "  <Default Extension=\"nuspec\" ContentType=\"application/octet\" />"
            "  <Default Extension=\"psmdcp\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\" />"
            "  <Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\" />"
            "</Types>"
        ]

        for (src, dst) in props.files do
            let dst = 
                if dst.EndsWith "/" then dst + Path.GetFileName(src)
                else dst
            let e = arch.CreateEntry(dst)
            let data = File.ReadAllBytes src
            use s = e.Open()
            s.Write(data, 0, data.Length)


        arch.Dispose()
        ms.ToArray()


    let installInCache (props : NupkgProperties) =
        let data = create props
        use ms = new MemoryStream(data)
        use a = new ZipArchive(ms, ZipArchiveMode.Read)

        let name = props.name.ToLower()

        let installDir = Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget", "packages", name, props.version)
        if not (Directory.Exists installDir) then Directory.CreateDirectory(installDir) |> ignore

        for e in a.Entries do
            let dst = e.FullName.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
            let dst = Path.Combine(Array.append [| installDir |] dst)

            let dir = Path.GetDirectoryName dst
            if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore

            let data =
                use s = e.Open()
                s.ReadToEnd()

            File.WriteAllBytes(dst, data)

        File.WriteAllBytes(Path.Combine(installDir, sprintf "%s.%s.nupkg" name props.version), data)

        use sha = SHA512.Create()
        let hash = sha.ComputeHash(data) |> System.Convert.ToBase64String
        File.WriteAllText(Path.Combine(installDir, sprintf "%s.%s.nupkg.sha512" name props.version), hash)

let test() =
    Nupkg.installInCache {
        name            = "Aardvark.Interactive"
        version         = "1.0.0"
        description     = "Aardvark extensions for dotnet-interactive"
        authors         = "Aardvark Platform Team"
        license         = License.Expression "MIT"
        url             = "about:blank"
        releaseNotes    = "none"
        dependencies    = 
            [
                "microsoft.dotnet.interactive", "[1.0.0-beta.20410.3]"
                "microsoft.dotnet.interactive.fsharp", "[1.0.0-beta.20410.3]"
                "Adaptify.Scripting.Compiler.Core", "[1.0.0-scripting0001]" 
            ]
        files =
            [
                @"C:\Users\Schorsch\Development\aardvark.interactive\bin\Debug\netcoreapp3.1\Aardvark.Interactive.dll", "interactive-extensions/dotnet/"
                @"C:\Users\Schorsch\Development\aardvark.interactive\bin\Debug\netcoreapp3.1\Aardvark.Interactive.dll", "lib/netcoreapp3.1/"
            ]
    }



