import Arizona from '@arizona-framework/client';

globalThis.arizona = new Arizona({ logLevel: 'debug' });
arizona.connect({ wsPath: '/live' });

document.addEventListener('arizonaEvent', (event) => {
    const { type, data } = event.detail;
    switch (type) {
        case 'status': {
            if (data.status === 'connected') {
                setTimeout(() => {
                    window.Prism.highlightAll();
                }, 50)
            }
            break;
        }
    }
});
