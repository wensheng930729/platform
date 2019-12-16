package com.bee.platform.user.controller;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.enums.FileType;
import com.bee.platform.user.service.OBSFileService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Arrays;
import java.util.List;

@Api(value = "华为云对象存储-API", tags = "华为云对象存储-API")
@RestController
@RequestMapping("/api/files")
@CrossOrigin(origins = "*")
public class OBSFileController {

	@Autowired
	private OBSFileService obsFileService;

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
	 * 
	 * @param file
	 * @param type
	 * @return
	 */
	private ResponseResult<JSONObject> uploadByType(MultipartFile file, FileType type) {
		String[] fileNames = {};
		String originalFilename = null;
		if (null != file.getOriginalFilename()) {
			originalFilename = file.getOriginalFilename();
			if (originalFilename != null && !"".equals(originalFilename)) {
				fileNames = originalFilename.split("\\.");
			}
		}
		// 文件名不能为空
		JSONObject json = new JSONObject();
		if (StringUtils.isBlank(originalFilename)) {
			json.put("message", "文件名称不能为空");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
		}
		// 获取文件后缀名
		String fileExtention = fileNames.length > 1 ? "." + fileNames[fileNames.length - 1].toLowerCase() : "";

		if (file.getSize() > 50 * 1024 * 1024) {
			json.put("message", "文件大小不能超过10M");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
		}
		// 限制上传图片类型
		if (FileType.IMAGE.equals(type)) {
			List<String> imageFormat = Arrays.asList(ConstantsUtil.IMAGE_FORMAT);
			// 校验文件格式
			if (!StringUtils.isEmpty(fileExtention) && !imageFormat.contains(fileExtention)) {
				json.put("message", "文件扩展名只能为jpg或png! 文件类型：" + fileExtention);
				return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
			}
		}
	
		return obsFileService.upload(file, fileExtention);
	}

}
