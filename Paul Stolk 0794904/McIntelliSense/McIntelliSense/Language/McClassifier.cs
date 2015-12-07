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
    [TagType(typeof(ClassificationTag))]
    internal sealed class McClassifierProvider : ITaggerProvider
    {

        [Export]
        [Name("mc")]
        [BaseDefinition("Intellisense")]
        internal static ContentTypeDefinition hidingContentTypeDefinition;

        [Export]
        [FileExtension(".mc")]
        [ContentType("mc")]
        internal static FileExtensionToContentTypeDefinition hiddenFileExtensionDefinition;


        [Import]
        internal IClassificationTypeRegistryService ClassificationTypeRegistry = null;

        [Import]
        internal IBufferTagAggregatorFactoryService aggregatorFactory = null;

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {

            ITagAggregator<McTokenTag> mcTagAggregator = aggregatorFactory.CreateTagAggregator<McTokenTag>(buffer);

            return new McClassifier(buffer, mcTagAggregator, ClassificationTypeRegistry) as ITagger<T>;
        }
    }

    internal sealed class McClassifier : ITagger<ClassificationTag>
    {
        ITextBuffer _buffer;
        ITagAggregator<McTokenTag> _aggregator;
        IDictionary<McTagEnum, IClassificationType> _mcTypes;

        internal McClassifier(ITextBuffer buffer, ITagAggregator<McTokenTag> mcTagAggregator, IClassificationTypeRegistryService typeService)
        {
            _buffer = buffer;
            _aggregator = mcTagAggregator;
            _mcTypes = new Dictionary<McTagEnum, IClassificationType>();
            _mcTypes[McTagEnum.Func] = typeService.GetClassificationType("func");
            _mcTypes[McTagEnum.Import] = typeService.GetClassificationType("import");
            _mcTypes[McTagEnum.TypeFunc] = typeService.GetClassificationType("typefunc");
            _mcTypes[McTagEnum.Data] = typeService.GetClassificationType("data");
            _mcTypes[McTagEnum.String] = typeService.GetClassificationType("string");
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged
        {
            add { }
            remove { }
        }

        public IEnumerable<ITagSpan<ClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {

            foreach (var tagSpan in this._aggregator.GetTags(spans))
            {
                var tagSpans = tagSpan.Span.GetSpans(spans[0].Snapshot);
                yield return new TagSpan<ClassificationTag>(tagSpans[0],new ClassificationTag(_mcTypes[tagSpan.Tag.type]));
            }
        }
    }
}
