#include <LListener.h>


class CPrologListener : public LListener {
public:
	CPrologListener(const char *pred);
	virtual void ListenToMessage(MessageT inMessage, void *ioParam);
protected:
	const char *predicate;
};
