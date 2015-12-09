using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System.Text.RegularExpressions;

namespace McSyntax
{
    [Export(typeof(ITaggerProvider))] 
    [ContentType("mc")]
    [TagType(typeof(McTokenTag))]
    internal sealed class OokTokenTagProvider : ITaggerProvider
    {

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            return new McTokenTagger(buffer) as ITagger<T>;
        }
    }

    public class McTokenTag : ITag
    {
        public McTagEnum type { get; private set; }

        public McTokenTag(McTagEnum type)
        {
            this.type = type;
        }
    }

    internal sealed class McTokenTagger : ITagger<McTokenTag>
    {
        ITextBuffer _buffer;
        IDictionary<string, McTagEnum> _mcTypes;
        
        static List<string> ListOfKeywords = new List<string>();
        internal McTokenTagger(ITextBuffer buffer)
        {
            _buffer = buffer;
            _mcTypes = new Dictionary<string, McTagEnum>();
            _mcTypes["func"] = McTagEnum.Func;
            _mcTypes["import"] = McTagEnum.Import;
            _mcTypes["typefunc"] = McTagEnum.TypeFunc;
            _mcTypes["data"] = McTagEnum.Data;
            _mcTypes["string"] = McTagEnum.String;
            _mcTypes["comment"] = McTagEnum.Comment;
            _mcTypes["signature"] = McTagEnum.Signature;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged
        {
            add { }
            remove { }
        }


        public static List<string> ListWithKeywords () {
            return ListOfKeywords;
        }

        public IEnumerable<ITagSpan<McTokenTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            bool commentIsActive = false;
            foreach (SnapshotSpan curSpan in spans)
            {
                ITextSnapshotLine containingLine = curSpan.Start.GetContainingLine();
                int curLoc = containingLine.Start.Position;

                foreach (Match match in Regex.Matches(containingLine.GetText(), "\".*?\""))
                {
                    curLoc = curLoc + match.Index;
                    var tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(curLoc, match.Length));
                    var data = match.Value.Trim('"');
                    ListOfKeywords.Add(data);
                    if (tokenSpan.IntersectsWith(curSpan))
                        yield return new TagSpan<McTokenTag>(tokenSpan, new McTokenTag(_mcTypes["string"]));
                }

                curLoc = containingLine.Start.Position;
                string[] tokens = containingLine.GetText().ToLower().Split(' ');
                foreach (string mcToken in tokens)
                {
                    if (_mcTypes.ContainsKey(mcToken))
                    {
                        if (!mcToken.Equals("string") && !mcToken.Equals("comment"))
                        {
                            var tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(curLoc, mcToken.Length));
                            if (tokenSpan.IntersectsWith(curSpan))
                                yield return new TagSpan<McTokenTag>(tokenSpan,
                                                                      new McTokenTag(_mcTypes[mcToken]));
                        }
                    }

                    //add an extra char location because of the space 
                    curLoc += mcToken.Length + 1;
                }

                curLoc = containingLine.Start.Position;
                var line = containingLine.GetText().ToLower();
                if(line.StartsWith("$$") || commentIsActive || line.StartsWith("$*") || line.StartsWith("*$") || line.EndsWith("*$"))
                {
                    if(line.EndsWith("*$"))
                    {
                        commentIsActive = false;
                    }

                    if(line.StartsWith("$*"))
                    {
                        commentIsActive = true;
                    }

                    var tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(curLoc, line.Length));
                    if (tokenSpan.IntersectsWith(curSpan))
                        yield return new TagSpan<McTokenTag>(tokenSpan, new McTokenTag(_mcTypes["comment"]));
                }
                //comments toevoegen
                //TODO Multiline comment lines between start and end token, not functional, see CommenIsActive bool

            }

        }
    }
}
