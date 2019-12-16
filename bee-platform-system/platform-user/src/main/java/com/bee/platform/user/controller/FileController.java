package com.bee.platform.user.controller;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.enums.FileType;
import com.qcloud.cos.COSClient;
import com.qcloud.cos.ClientConfig;
import com.qcloud.cos.request.UploadFileRequest;
import com.qcloud.cos.sign.Credentials;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

/**
 * Created by Aaron on 11/2/16.
 */
@Slf4j
@Api(value = "腾讯云上传图片-API", tags = "腾讯云图片上传相关API")
@RestController
@RequestMapping("/old/api/files")
@CrossOrigin(origins = "*")
public class FileController {

    @Value("${qcloud.cos.appId}")
    private String appId;
    @Value("${qcloud.cos.secretId}")
    private String secretId;
    @Value("${qcloud.cos.secretKey}")
    private String secretKey;
    @Value("${qcloud.cos.bucketName}")
    private String bucketName;
    @Value("${qcloud.cos.imageLocalpath}")
    private String imageLocalpath;

    @ApiOperation(value = "上传图片", notes = "上传图片")
    @RequestMapping(value = "/upload", method = RequestMethod.POST)
    public ResponseResult<JSONObject> upload(@RequestPart MultipartFile file) {
        return uploadByType(file, FileType.IMAGE);
    }

    @ApiOperation(value = "上传文件", notes = "上传文件")
    @RequestMapping(value = "/uploadFile", method = RequestMethod.POST)
    public ResponseResult<JSONObject> uploadFile(@RequestPart MultipartFile file) {
        return uploadByType(file, FileType.FILE);
    }

    /**
     * 统一上传接口
     * @param file
     * @param type
     * @return
     */
    private ResponseResult<JSONObject> uploadByType(MultipartFile file, FileType type) {
        String[] fileNames  = {};
        String originalFilename =null;
        if(null != file.getOriginalFilename()) {
            originalFilename = file.getOriginalFilename();
            if(originalFilename!=null && !"".equals(originalFilename)) {
                fileNames = originalFilename.split("\\.");
            }
        }
        //文件名不能为空
        JSONObject json = new JSONObject();
        if(StringUtils.isBlank(originalFilename)){
            json.put("message", "文件名称不能为空" );
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER,json);
        }
        //获取文件后缀名
        String fileExtention = fileNames.length > 1 ? "." + fileNames[fileNames.length-1].toLowerCase() : "";

        if(file.getSize() > 10 * 1024 * 1024){
            json.put("message", "文件大小不能超过10M" );
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER,json);
        }
        //限制上传图片类型
        if(FileType.IMAGE.equals(type)) {
            List<String> imageFormat= Arrays.asList(ConstantsUtil.IMAGE_FORMAT);
            //校验文件格式
            if(!StringUtils.isEmpty(fileExtention)&&!imageFormat.contains(fileExtention)){
                json.put("message", "文件扩展名只能为jpg或png! 文件类型："+fileExtention );
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
            }
        }
        String fileUUid = UUID.randomUUID().toString().replace("-","");
        File tempFile = new File(imageLocalpath+fileUUid+fileExtention);
        if(!tempFile.exists()){
            try {
                tempFile.createNewFile();
                file.transferTo(tempFile);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        // 初始化秘钥信息
        long id = Long.parseLong(appId);
        Credentials cred = new Credentials(id, secretId, secretKey);
        // 初始化客户端配置
        ClientConfig clientConfig = new ClientConfig();
        // 设置bucket所在的区域，比如广州(gz), 天津(tj)
        clientConfig.setRegion("gz");
        // 初始化cosClient
        COSClient cosClient = new COSClient(clientConfig, cred);

        String uploadFileRet= "";
        try {
            //调用腾讯云上传接口，返回CDN图片保存地址
            UploadFileRequest uploadFileRequest = new UploadFileRequest(bucketName,"/"+fileUUid+fileExtention,imageLocalpath+fileUUid+fileExtention);
            uploadFileRet = cosClient.uploadFile(uploadFileRequest);
            JSONObject jsonObject = JSONObject.parseObject(uploadFileRet);
            if(jsonObject!=null) {
                String code = jsonObject.getString("code");
                if(!ConstantsUtil.FALSE.equals(code)) {
                    json.put("message", "调用腾讯云接口出错" );
                    return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND,json);
                }else {
                    json = jsonObject.getJSONObject("data");
            		//前端页面解析http报错 统一将返回的http转换成https返回
                    String jsonStr = json.toString();
                    jsonStr = jsonStr.replaceAll("http", "https");
                    json = JSONObject.parseObject(jsonStr);
                }
            }
        } catch (Exception e) {
            log.error("调用腾讯云上传接口异常，异常信息是:{}。返回信息是：{}",e,JSONObject.toJSONString(uploadFileRet));
        }
        tempFile.delete();
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);
    }

}
