package ru.vitkud.delphi;

import ru.vitkud.delphi.java.DjUnit;
import ru.vitkud.delphi.java.JavaProjectFileAppender;

import javax.swing.*;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.Document;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import java.awt.*;
import java.awt.event.*;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Delphi2JavaGui extends JDialog {
	private JPanel contentPane;
	private JButton buttonOK;
	private JButton buttonCancel;
	private JTextArea txaDelphi;
	private JTextArea txaJava;

	private static final String TEMP_SOURCE_DELPHI_FILE_PATH = "delphi_source.tmp";

	private UndoManager undoManager;


	// Set system look and feels
	{
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e) {
			e.printStackTrace();
		}
	}

	public Delphi2JavaGui() {
		setContentPane(contentPane);
		setModal(true);
		getRootPane().setDefaultButton(buttonOK);

		buttonOK.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onOK();
			}
		});

		buttonCancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onCancel();
			}
		});

// call onCancel() when cross is clicked
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				onCancel();
			}
		});

// call onCancel() on ESCAPE
		contentPane.registerKeyboardAction(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				onCancel();
			}
		}, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

		// Read temp source
		try {
			txaDelphi.setText(new String(Files.readAllBytes(Paths.get(TEMP_SOURCE_DELPHI_FILE_PATH)),
					StandardCharsets.UTF_8));
		} catch (IOException e) {
			e.printStackTrace();
		}

		// UndoManager
		undoManager = new UndoManager();
		Document doc = txaDelphi.getDocument();
		doc.addUndoableEditListener(new UndoableEditListener() {
			@Override
			public void undoableEditHappened(UndoableEditEvent e) {
				undoManager.addEdit(e.getEdit());
			}
		});

		InputMap im = txaDelphi.getInputMap(JComponent.WHEN_FOCUSED);
		ActionMap am = txaDelphi.getActionMap();

		im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Z, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "Undo");
		im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Y, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "Redo");

		am.put("Undo", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					if (undoManager.canUndo()) {
						undoManager.undo();
					}
				} catch (CannotUndoException exp) {
					exp.printStackTrace();
				}
			}
		});
		am.put("Redo", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					if (undoManager.canRedo()) {
						undoManager.redo();
					}
				} catch (CannotUndoException exp) {
					exp.printStackTrace();
				}
			}
		});
	}

	private void onOK() {
// add your code here
		try {
			DelphiProject delphiProject = new DelphiProject();
			delphiProject.setDefinitions(new String[]{"VER220", "RELEASE"});
			DelphiUnit delphiUnit = new DelphiUnit(delphiProject, new ByteArrayInputStream(txaDelphi.getText().getBytes()));

			JavaProjectFileAppender projectAppender = new JavaProjectFileAppender() {
				@Override
				public void appendMainJava(String fullPackage, String className, String content) {
					txaJava.append(fullPackage.replace('.', '/') + "/" + className + ".java");
					txaJava.append("\n----------------------------------------\n");
					txaJava.append(content);
					txaJava.append("\n");
				}

				@Override
				public void appendTestJava(String fullPackage, String className, String content) {
					appendMainJava(fullPackage, className, content);
				}
			};
			DjUnit djUnit = new DjUnit(delphiUnit);
			djUnit.toJava(projectAppender, "");
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Write temp source
		try (PrintWriter out = new PrintWriter(TEMP_SOURCE_DELPHI_FILE_PATH)) {
			out.write(txaDelphi.getText());
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

	private void onCancel() {
// add your code here if necessary
		dispose();
	}

	public static void main(String[] args) {
		Delphi2JavaGui dialog = new Delphi2JavaGui();
		dialog.pack();
		dialog.setVisible(true);
		System.exit(0);
	}

	private void createUIComponents() {
		// TODO: place custom component creation code here
	}

	{
// GUI initializer generated by IntelliJ IDEA GUI Designer
// >>> IMPORTANT!! <<<
// DO NOT EDIT OR ADD ANY CODE HERE!
		$$$setupUI$$$();
	}

	/**
	 * Method generated by IntelliJ IDEA GUI Designer
	 * >>> IMPORTANT!! <<<
	 * DO NOT edit this method OR call it in your code!
	 *
	 * @noinspection ALL
	 */
	private void $$$setupUI$$$() {
		contentPane = new JPanel();
		contentPane.setLayout(new BorderLayout(0, 0));
		final JSplitPane splitPane1 = new JSplitPane();
		splitPane1.setResizeWeight(0.5);
		contentPane.add(splitPane1, BorderLayout.CENTER);
		final JScrollPane scrollPane1 = new JScrollPane();
		splitPane1.setLeftComponent(scrollPane1);
		txaDelphi = new JTextArea();
		txaDelphi.setColumns(40);
		txaDelphi.setFont(new Font("Courier New", txaDelphi.getFont().getStyle(), 12));
		txaDelphi.setRows(10);
		scrollPane1.setViewportView(txaDelphi);
		final JScrollPane scrollPane2 = new JScrollPane();
		splitPane1.setRightComponent(scrollPane2);
		txaJava = new JTextArea();
		txaJava.setColumns(40);
		txaJava.setFont(new Font("Courier New", txaJava.getFont().getStyle(), 12));
		txaJava.setRows(10);
		scrollPane2.setViewportView(txaJava);
		final JPanel panel1 = new JPanel();
		panel1.setLayout(new BorderLayout(0, 0));
		contentPane.add(panel1, BorderLayout.SOUTH);
		panel1.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4), null));
		final JPanel panel2 = new JPanel();
		panel2.setLayout(new BorderLayout(0, 0));
		panel1.add(panel2, BorderLayout.EAST);
		buttonOK = new JButton();
		buttonOK.setText("OK");
		panel2.add(buttonOK, BorderLayout.WEST);
		buttonCancel = new JButton();
		buttonCancel.setText("Cancel");
		panel2.add(buttonCancel, BorderLayout.CENTER);
	}

	/**
	 * @noinspection ALL
	 */
	public JComponent $$$getRootComponent$$$() {
		return contentPane;
	}
}
