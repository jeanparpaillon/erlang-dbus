#include <QCoreApplication>
#include <QtDBus/QtDBus>
#include <QDebug>

class SampleObject : public QObject
{
    Q_OBJECT
    Q_CLASSINFO("D-Bus Interface", "net.lizenn.dbus.SampleInterface")

public:
    SampleObject(QObject *parent = nullptr) : QObject(parent) {}

public slots:
    Q_SCRIPTABLE QStringList HelloWorld(const QString &helloMessage) {
        qDebug() << helloMessage;
        emit SampleSignal(42, 24);
        emit SampleSignal2();
        return QStringList() << "Hello World" << " from example-service";
    }

    Q_SCRIPTABLE QStringList GetTuple() {
        return QStringList() << "Hello Tuple" << " from example-service";
    }

    Q_SCRIPTABLE QStringList GetTuple(const QString &message) {
        return QStringList() << message << " from example-service";
    }

    Q_SCRIPTABLE QVariantMap GetDict() {
        QVariantMap map;
        map["first"] = "Hello Dict";
        map["second"] = " from example-service";
        return map;
    }

    Q_SCRIPTABLE QString GetString(uint size) {
        return QString(size, 'x');
    }

signals:
    Q_SCRIPTABLE void SampleSignal(int x, int y);
    Q_SCRIPTABLE void SampleSignal2();
};

int main(int argc, char *argv[])
{
    QCoreApplication app(argc, argv);

    if (!QDBusConnection::sessionBus().isConnected()) {
        qCritical() << "Cannot connect to the D-Bus session bus";
        return 1;
    }

    if (!QDBusConnection::sessionBus().registerService("net.lizenn.dbus.SampleService")) {
        qCritical() << "Cannot register the service:" << QDBusConnection::sessionBus().lastError().message();
        return 1;
    }

    SampleObject root, child1, little1, little2;

    QDBusConnection::sessionBus().registerObject("/root", &root, QDBusConnection::ExportAllContents);
    QDBusConnection::sessionBus().registerObject("/root/child1", &child1, QDBusConnection::ExportAllContents);
    QDBusConnection::sessionBus().registerObject("/root/child2/little1", &little1, QDBusConnection::ExportAllContents);
    QDBusConnection::sessionBus().registerObject("/root/child2/little2", &little2, QDBusConnection::ExportAllContents);

    qDebug() << "Service started";
    return app.exec();
}

#include "main.moc"
