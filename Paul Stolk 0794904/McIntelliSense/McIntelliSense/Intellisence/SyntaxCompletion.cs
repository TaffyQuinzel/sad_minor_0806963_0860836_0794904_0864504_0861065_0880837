using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Operations;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows;
using System.Reflection;
using Microsoft.Internal.VisualStudio.PlatformUI;
using System.IO;

namespace McSyntax.Intellisence
{
    internal class SyntaxCompletion : ICompletionSource
    {
        private SyntaxCompletionProvider m_sourceProvider;
        private ITextBuffer m_textBuffer;
        private List<Completion> m_compList;

        public SyntaxCompletion(SyntaxCompletionProvider sourceProvider, ITextBuffer textBuffer)
        {
            m_sourceProvider = sourceProvider;
            m_textBuffer = textBuffer;
        }

        void ICompletionSource.AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            List<string> strList = new List<string>();
            strList.Add("Func");
            strList.Add("TypeFunc");
            strList.Add("Import");
            strList.Add("Data");
            var visualsnapshot = session.TextView.VisualSnapshot;

            var KeywordsuserDefined = new List<string>();
            foreach (var line in visualsnapshot.Lines)
            {
                var textLine = line.Extent.GetText();
                if (textLine.StartsWith("import "))
                {
                    //do something
                    var openDocument = m_textBuffer.Properties.GetProperty(typeof(ITextDocument));
                    if (openDocument != null)
                    {
                        //var file = (ITextDocument)openDocument;
                        //var directory = Path.GetDirectoryName(file.FilePath);
                        //var filename = Path.GetFileName(file.FilePath);
                        //if current file in folder 'standard' 
                        //eerst check if file in same folder as current file
                        //als   
                       // var paddefault = Environment.GetEnvironmentVariable("MC_HOME");
                    }

                }

                foreach (Match match in Regex.Matches(textLine, "\".*?\""))
                {
                    var data = match.Value.Trim('"');
                    KeywordsuserDefined.Add(data);
                }
            }

            KeywordsuserDefined = KeywordsuserDefined.Distinct().ToList();
            strList.AddRange(KeywordsuserDefined);
            strList.Add("Signature");
            m_compList = new List<Completion>();
            foreach (string str in strList.OrderBy(s => s))
                m_compList.Add(new Completion(str, str, str, null, null));

            completionSets.Add(new CompletionSet(
                "Tokens",    //the non-localized title of the tab
                "Tokens",    //the display title of the tab
                FindTokenSpanAtPosition(session.GetTriggerPoint(m_textBuffer),
                    session),
                m_compList,
                null));
        }

        private ITrackingSpan FindTokenSpanAtPosition(ITrackingPoint point, ICompletionSession session)
        {
            SnapshotPoint currentPoint = (session.TextView.Caret.Position.BufferPosition) - 1;
            ITextStructureNavigator navigator = m_sourceProvider.NavigatorService.GetTextStructureNavigator(m_textBuffer);
            TextExtent extent = navigator.GetExtentOfWord(currentPoint);
            return currentPoint.Snapshot.CreateTrackingSpan(extent.Span, SpanTrackingMode.EdgeInclusive);
        }

        private bool m_isDisposed;
        public void Dispose()
        {
            if (!m_isDisposed)
            {
                GC.SuppressFinalize(this);
                m_isDisposed = true;
            }
        }
    }
}
