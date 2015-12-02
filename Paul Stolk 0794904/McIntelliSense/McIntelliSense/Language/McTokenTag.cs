using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

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

        internal McTokenTagger(ITextBuffer buffer)
        {
            _buffer = buffer;
            _mcTypes = new Dictionary<string, McTagEnum>();
            _mcTypes["func"] = McTagEnum.Func;
            _mcTypes["import"] = McTagEnum.Import;
            _mcTypes["typefunc"] = McTagEnum.TypeFunc;
            _mcTypes["data"] = McTagEnum.Data;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged
        {
            add { }
            remove { }
        }

        public IEnumerable<ITagSpan<McTokenTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {

            foreach (SnapshotSpan curSpan in spans)
            {
                ITextSnapshotLine containingLine = curSpan.Start.GetContainingLine();
                int curLoc = containingLine.Start.Position;
                string[] tokens = containingLine.GetText().ToLower().Split(' ');

                foreach (string mcToken in tokens)
                {
                    if (_mcTypes.ContainsKey(mcToken))
                    {
                        var tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(curLoc, mcToken.Length));
                        if (tokenSpan.IntersectsWith(curSpan))
                            yield return new TagSpan<McTokenTag>(tokenSpan,
                                                                  new McTokenTag(_mcTypes[mcToken]));
                    }

                    //add an extra char location because of the space 
                    curLoc += mcToken.Length + 1;
                }
            }

        }
    }
}
