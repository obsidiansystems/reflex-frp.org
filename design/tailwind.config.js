module.exports = {
  purge: [],
  theme: {
    fontFamily: {
      display: ['DM Serif Display', 'sans-serif'],
      body: ['Poppins', 'sans-serif'],
      sans: ['Poppins', 'sans-serif'],
      mono: ['Roboto Mono', 'monospace']
    },
    extend: {
      maxWidth: {
        'viewport': '100vp'
      },
      maxHeight: {
        '64': '16rem',
        '72': '18rem',
        '80': '20rem',
        '84': '22rem',
        '92': '24rem',
        '100': '26rem',
        '116': '30rem',
        '132': '34rem',
        '140': '38rem'
      },
      fontSize: {
        '8xl': '5rem',
        '10xl': '6rem',
      },
      borderWidth: {
        '3': '3px'
      }
    },
  },
  variants: {
    boxShadow: ['responsive', 'hover', 'focus', 'active'],
    borderColor: ['responsive', 'hover', 'focus', 'active'],
  },
  plugins: [
    require('@tailwindcss/ui'),
  ],
}
