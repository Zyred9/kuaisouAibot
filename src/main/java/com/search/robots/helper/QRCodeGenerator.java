package com.search.robots.helper;

import cn.hutool.core.util.StrUtil;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.WriterException;
import com.google.zxing.client.j2se.MatrixToImageConfig;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

/**
 * 二维码生成工具类 (基于 ZXing 库)
 * 增加了在图片上绘制文字的功能
 */
public class QRCodeGenerator {
//
//    public static void main(String[] args) {
//
//        // 1. 设置参数
//        String qrContent = "TH4C92EC3M6fB1unShCjpvCQbpjko4YeDU"; // 二维码编码的内容
//        String headerID = "ID:(7653000728)"; // 要显示在图片顶部的ID
//        String footerText = qrContent; // 暂时不添加底部文字，如果需要可以设置
//        int qrWidth = 400;  // 二维码部分的宽度
//        int qrHeight = 400; // 二维码部分的高度
//        String filePath = "generated_qr_code_with_id.png"; // 生成的文件名和路径
//
//        try {
//            // 2. 调用工具类生成带ID的二维码
//            File qrCodeFile = QRCodeGenerator.generateQRCodeImageWithText(
//                    qrContent,
//                    headerID,
//                    footerText,
//                    qrWidth,
//                    qrHeight,
//                    filePath
//            );
//
//            // 3. 打印结果
//            if (qrCodeFile.exists()) {
//                System.out.println("✅ 带ID的二维码生成成功！");
//                System.out.println("文件路径: " + qrCodeFile.getAbsolutePath());
//                System.out.println("文件大小: " + qrCodeFile.length() + " 字节");
//            } else {
//                System.out.println("❌ 带ID的二维码生成失败。");
//            }
//
//        } catch (WriterException e) {
//            System.err.println("二维码编码失败：" + e.getMessage());
//        } catch (IOException e) {
//            System.err.println("文件写入失败：" + e.getMessage());
//        }
//    }

    public static File buildQrCode (String address, Long userId, String prefix) {
        try {
            String path = prefix + File.separator + address + ".png" ;
            return QRCodeGenerator.generateQRCodeImageWithText(
                    address,
                    StrUtil.format("ID:({})", userId),
                    address,
                    path
            );
        } catch (WriterException | IOException e) {
            return null;
        }
    }


    /**
     * 根据内容生成二维码图片文件，并在图片上方或下方添加文字
     *
     * @param content        要编码到二维码中的字符串内容
     * @param headerText     要添加到图片顶部的文字 (如果为 null 或空则不添加)
     * @param footerText     要添加到图片底部的文字 (如果为 null 或空则不添加)
     * @param filePathAndName 要保存的文件路径和名称
     * @return 生成的 File 对象
     * @throws WriterException 二维码编码失败时抛出
     * @throws IOException     文件写入失败时抛出
     */
    public static File generateQRCodeImageWithText(String content, String headerText, String footerText, String filePathAndName)
            throws WriterException, IOException {

        // 1. 设置编码参数
        Map<EncodeHintType, Object> hints = new HashMap<>();
        hints.put(EncodeHintType.CHARACTER_SET, "UTF-8");
        hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M); // 错误纠正等级
        hints.put(EncodeHintType.MARGIN, 1); // 二维码的边框留白

        int qrCodeWidth = 400, qrCodeHeight = 400;

        // 2. 生成 BitMatrix (纯二维码位矩阵)
        MultiFormatWriter writer = new MultiFormatWriter();
        BitMatrix bitMatrix = writer.encode(
                content,
                BarcodeFormat.QR_CODE,
                qrCodeWidth,
                qrCodeHeight,
                hints
        );

        // 3. 将 BitMatrix 转换为 BufferedImage
        // 使用 MatrixToImageConfig 可以指定前景色和背景色
        BufferedImage qrImage = MatrixToImageWriter.toBufferedImage(bitMatrix,
                new MatrixToImageConfig(0xFF000000, 0xFFFFFFFF)); // 黑色前景，白色背景

        // 4. 计算图片总高度以容纳文字
        int textMargin = 10; // 文字与二维码的间距
        int fontSize = 20;   // 文字大小

        int headerHeight = 0;
        if (headerText != null && !headerText.trim().isEmpty()) {
            headerHeight = fontSize + textMargin; // 预留文字高度 + 边距
        }

        int footerHeight = 0;
        if (footerText != null && !footerText.trim().isEmpty()) {
            footerHeight = fontSize + textMargin; // 预留文字高度 + 边距
        }

        int totalHeight = qrCodeHeight + headerHeight + footerHeight;

        // 5. 创建一个新的 BufferedImage 来承载二维码和文字
        BufferedImage finalImage = new BufferedImage(qrCodeWidth, totalHeight, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = finalImage.createGraphics();

        // 填充背景为白色
        graphics.setColor(Color.WHITE);
        graphics.fillRect(0, 0, qrCodeWidth, totalHeight);

        // 设置抗锯齿，使文字更平滑
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

        // 6. 绘制顶部文字
        int currentY = 0;
        if (headerText != null && !headerText.trim().isEmpty()) {
            graphics.setColor(Color.BLACK); // 文字颜色
            graphics.setFont(new Font("宋体", Font.PLAIN, fontSize)); // 字体和大小

            // 计算文字居中位置
            FontMetrics fm = graphics.getFontMetrics();
            int textWidth = fm.stringWidth(headerText);
            int x = (qrCodeWidth - textWidth) / 2;
            int y = fm.getAscent() + (headerHeight - fm.getHeight()) / 2; // 调整为居中对齐
            graphics.drawString(headerText, x, y);
            currentY = headerHeight;
        }

        // 7. 绘制二维码
        graphics.drawImage(qrImage, 0, currentY, null);
        currentY += qrCodeHeight;

        // 8. 绘制底部文字
        if (footerText != null && !footerText.trim().isEmpty()) {
            graphics.setColor(Color.BLACK);
            graphics.setFont(new Font("宋体", Font.PLAIN, fontSize));

            FontMetrics fm = graphics.getFontMetrics();
            int textWidth = fm.stringWidth(footerText);
            int x = (qrCodeWidth - textWidth) / 2;
            int y = currentY + fm.getAscent() + (footerHeight - fm.getHeight()) / 2;
            graphics.drawString(footerText, x, y);
        }

        graphics.dispose(); // 释放 Graphics2D 资源

        // 9. 将最终图片写入文件
        Path path = FileSystems.getDefault().getPath(filePathAndName);
        ImageIO.write(finalImage, "PNG", path.toFile());

        // 10. 返回 File 对象
        return path.toFile();
    }
}