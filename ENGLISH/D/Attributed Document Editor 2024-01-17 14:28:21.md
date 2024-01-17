```d
import std.stdio, std.random, std.algorithm, std.range;
import std.algorithm.sequence;
import std.container.map, std.rational, std.traits;
import std.typecons, std.traits.minmax;
import std.typecons.stdatomic;

template Document(in mutable(Document) mainDocument) {
    auto doc = mainDocument;
    immutable docState = doc;

    struct Component {
        typedef Atomic!immutable(Component, docState) State;

        immutable State state; // Point into docState at the associated history
        immutable Position top, bottom; // Where I am located in the main document

        Component(immutable State state, immutable Position top, immutable Position bottom)
            : state(state), top(top), bottom(bottom) {}

        enum DocumentChange { insert, update, remove } change {
            if (state.deleted) remove;
            else if (state.new) insert;
            else update;
        }

        immutable Position start() const pure nothrow { return top; }
        immutable Position end() const pure nothrow { return bottom; }
        immutable bool isDeleted() const pure nothrow { return state.deleted; }
        immutable Rational size() const pure nothrow {
            return Rational((end() - start()));
        }
    };

    struct ComponentComparator {
        int compare(const immutable(Component) a, const immutable(Component) b) const nothrow {
            return a.start() compareTo b.start();
        }
    };

    struct DocumentState {
        typedef Atomic!immutable(DocumentState, docState) State;
        immutable State state; // Point into the main document's history

        immutable Position start, end; // Start and end position of this document state
        mutable Component[] components; // Components of the document state
        mutable!int componentCount;
        mutable(Component, docState) lastDelete;

        void swap(out DocumentState other) pure nothrow {
            this.state.swap(other.state);
            this.start.swap(other.start);
            this.end.swap(other.end);
            this.componentCount.swap(other.componentCount);
            this.lastDelete.swap(other.lastDelete);
            this.components.swap(other.components);
        }

        DocumentState(immutable State state, immutable Position start, immutable Position end)
            : state(state), start(start), end(end), components[] {
        }
    };

    struct InsertChange {
        immutable Position start, end;

        InsertChange(immutable Position start, immutable Position end)
            : start(start), end(end) {
        }
    };

    struct UpdateChange {
        immutable Position start, end, start2, end2;

        UpdateChange(in Position start, in Position end, in Position start2, in Position end2)
            : start(start), end(end), start2(start2), end2(end2) {
        }
    };

    struct RemoveChange {
        immutable Position start, end;

        RemoveChange(immutable Position start, immutable Position end)
            : start(start), end(end) {
        }
    };

    DocumentChange(in DocumentState docState, in immutable Position start, in immutable Position end)
        : docState(docState), start(start), end(end) {
    }

    InsertChange makeInsert(in Position start, in Position end) {
        return new InsertChange(start, end);
    }

    UpdateChange makeUpdate(in Position start, in Position end, in Position start2, in Position end2) {
        return new UpdateChange(start, end, start2, end2);
    }

    RemoveChange makeRemove(in Position start, in Position end) {
        return new RemoveChange(start, end);
    }

    immutable(DocumentChange)[] mergeChanges(immutable(DocumentChange)[] a, in immutable(DocumentChange)[] b) {
        immutable(DocumentChange)[] result = new(a.length + b.length);
        size_t ai = 0, bi = 0, ri = 0;

        while (ai < a.length && bi < b.length) {
            auto cA = a[ai], cB = b[bi];
            if (cA.start < cB.start) {
                result[ri++] = cA;
                ai++;
            } else {
                result[ri++] = cB;
                bi++;
            }
        }

        while (ai < a.length) {
            result[ri++] = a[ai++];
        }

        while (bi < b.length) {
            result[ri++] = b[bi++];
        }

        result.length = ri;
        return result;
    }

    immutable(DocumentChange)[] sortChanges(immutable(DocumentChange)[] changes) {
        immutable(DocumentChange)[] result;
        Range.sort(result, changes, ComponentComparator());
        return result;
    }

    immutable Position computeDocStateEnd(immutable Position start, immutable(DocumentChange)[] changes) pure {
        foreach (ch; changes) {
            switch (ch.change) {
                case DocumentChange.insert:
                    start += (ch.end - ch.start);
                    break;
                case DocumentChange.update:
                    if (start < ch.start || (start > ch.start && start < ch.end)) start += (ch.end2 - ch.end);
                    break;
                case DocumentChange.remove:
                    if (start < ch.start) break;
                    else if (start < ch.end) start = 0;
                    else start -= (ch.end - ch.start);
                    break;
            }
        }
        return start;
    }

    void rollbackComponents(immutable(DocumentChange)[] changes) {
        foreach (ch; changes) {
            switch (ch.change) {
                case DocumentChange.insert:
                    doc.removeRange(ch.start, ch.end);
                    break;
                case DocumentChange.update:
                    doc.removeRange(ch.start, ch.end);
                    doc.insertString(ch.start2, ch.end2, doc.string);
                    break;
                case DocumentChange.remove:
                    doc.insertString(ch.start, ch.end, doc.string);
                    break;
            }
        }
    }

    immutable(DocumentChange)[] makeStateChanges(in immutable(DocumentChange)[] changes) pure {
        immutable(DocumentChange)[] result = new(changes.length);
        size_t ri = 0;

        foreach (change; changes) {
            immutable Position start = change.start, end = change.end;
            result[ri++] = DocumentChange!makeUpdate(start, end, start, end);
        }
        return result;
    }

    void applyComponent(immutable Position currentComponentStart, immutable(Component) comp) {
        switch (comp.change) {
            case DocumentChange.insert:
                doc.insertString(comp.start, comp.end, comp.state.string);
                doc.insertComponent(currentComponentStart, comp);
                break;
            case DocumentChange.update:
                doc.removeRange(comp.start, comp.end);
                doc.insertString(comp.start2, comp.end2, comp.state.string);
                doc.replaceComponent(currentComponentStart, comp);
                break;
            case DocumentChange.remove:
                doc.removeRange(comp.start, comp.end);
                doc.removeComponent(currentComponentStart);
                break;
        }
    }

    void applyChangesToState(immutable(DocumentChange)[] changes, out DocumentState docState) {
        immutable Position start = docState.start;
        immutable(Component)[] components = docState.components;

        size_t ri = 0, ci = 0;

        while (ri < changes.length && ci < components.length) {
            auto ch = changes[ri], comp = components[ci];
            if (ch.start < comp.start) {
                ri++;
            } else if (ch.start > comp.start) {
                ci++;
            } else {
                applyComponent(ci, comp);
                ri++;
                ci++;
            }
        }

        while (ri < changes.length) {
            applyComponent(components.length, changes[ri].component);
            ri++;
        }

        while (ci < components.length) {
            immutable Position start = components[ci].start, end = components[ci].end;
            doc.removeRange(start, end);
            doc.removeComponent(ci);
            docState.componentCount--;
        }

        docState.end = computeDocStateEnd(docState.start, changes);
    }

    void applyHistoryChanges(immutable(DocumentChange)[] changes) {
        immutable(DocumentChange)[] allChanges = mergeChanges(doc.historyChanges, changes);
        allChanges.length = sortChanges(allChanges).length;
        applyChangesToState(makeStateChanges(allChanges), docState);
        doc.historyChanges = allChanges;
        doc.clearHistoryChanges();
    }

    void applyChanges(immutable(DocumentChange)[] changes) {
        applyChangesToState(changes, docState);
        doc.removeHistory(docState.state);
        doc.addState(docState.state);
        doc.clearHistoryChanges();
        doc.updateComponentCountWith(docState.componentCount);
        doc.updateEnd();
    }

    void clear() {
        doc.components = new Component[];
        doc.historyChanges = new(DocumentChange) [];
        doc.historyStates = new State[];

        docState = new DocumentState(State(), 0, 0);
        docState.componentCount = 0;
    }

    void insertString(in Position pos, in string text) {
        Position start = doc.start + pos;
        Position end = start + text.length;
        applyChanges(new(makeInsert(start, end))[]);
    }

    void removeRange(in Position start, in Position end) {
        Position stateStart = docState.start, stateEnd = docState.end;
        foreach (i; 0 .. componentCount) {
            auto comp = components[i];
            if (comp.start >= end || comp.end <= start) continue;

            Position start2 = Comp.start, startChange = start;
            Position end2 = Comp.end, endChange = end;

            if (comp.start < start) {
                startChange = comp.start;
                start2 = start;
            }

            if (comp.end > end) {
                endChange = comp.end;
                end2 = end;
            }

            immutable(DocumentChange)[] changes = new(makeUpdate(startChange, endChange, start2, end2))[];
            applyChanges(changes);
        }

        Position stateStart2 = 0, stateEnd2 = 0;

        if (stateStart < start) {
            stateStart2 = stateStart;
            stateStart = start;
        }

        if (stateEnd > end) {
            stateEnd2 = stateEnd;
            stateEnd = end;
        }

        applyChanges(makeRemove(stateStart, stateEnd));

        immutable(DocumentChange)[] stateChanges = new(DocumentChange!makeUpdate(start, end, stateStart2, stateEnd2))[];
        applyChanges(stateChanges);
    }

    size_t componentCount() {
        return docState.componentCount;
    }

    void updateComponentCountWith(in size_t count) {
        docState.componentCount = count;
    }

    void insertComponent(in size_t pos, in Component comp) {
        immutable Position start = doc.start + pos;
        immutable Position end = comp.end;
        immutable(DocumentChange)[] changes = new(makeInsert(start, end))[];
        applyChanges(changes);
        docState.componentCount++;
    }

    void replaceComponent(in size_t pos, in Component comp) {
        immutable Position start = doc.start + pos;
        immutable Position end = comp.end;
        immutable(DocumentChange)[] changes = new(makeUpdate(start, comp.end, start, end))[];
        applyChanges(changes);
    }

    void removeComponent(in size_t pos) {
        immutable Position start = doc.start + pos;
        immutable Position end = components[pos].end;
        immutable(DocumentChange)[] changes = new(makeRemove(start, end))[];
        applyChanges(changes);
        docState.componentCount--;
    }

    void clearHistoryChanges() {
        immutable(DocumentChange)[] temp = new(0);
        doc.historyChanges.length = temp.length;
    }

    Position computeEnd() {
        return computeDocStateEnd(docState.start, historyChanges);
    }

    void updateEnd() {
        doc.end = computeEnd();
    }

    void addState(in State state) {
        docState = new DocumentState(state, docState.start, docState.end);
        doc.historyStates.length++;
    }

    void removeState(in State state) {
        Position statePos1;
        foreach (i; 0 .. historyStates.length) {
            if (historyStates[i] == state) {
                statePos1 = i;
                break;
            }
        }

        Position statePos2;
        foreach (i; statePos1 .. historyStates.length) {
            immutable(DocumentChange)[] changes = new(DocumentChange!makeUpdate(docStates[i].start, docStates[i].end, 0, 0))[];
            rollbackComponents(changes);
            docStates[i] = docStates[i-1];
            historyChanges[i] = historyChanges[i-1];
            if (docStates[i].state == state) {
                statePos2 = i;
                break;
            }
        }

        docStates.length -= statePos2 - statePos1 + 1;
        historyStates.length -= statePos2 - statePos1 + 1;
        historyChanges.length -= statePos2 - statePos1 + 1;
    }

    void removeHistory(in State state) {
        immutable(DocumentChange)[] changes;
        while (docState.state != state) {
            changes = new(DocumentChange!makeUpdate(docState.start, docState.end, 0, 0))[];
            rollbackComponents(changes);
            docState = new DocumentState(docState.state - 1,
                                        docState.start,
                                        docState.end);
        }

        docStates.length = docState.state + 1;
        historyStates.length = docState.state + 1;
        historyChanges.length = docState.state + 1;
    }
}