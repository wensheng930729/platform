package com.bee.platform.datadriver.controller;


import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.EnterprisesUsersInfoDTO;
import com.bee.platform.user.service.feign.EnterprisesUsersFeiginClient;

import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping("/erpUserManagement")
public class ErpUserManagementController {
	@Autowired
	private EnterprisesUsersFeiginClient enterprisesUsersFeiginClient;

	@PostMapping("/add")
    @ApiOperation(value = "添加中台用户")
    public ResponseResult add(@RequestParam("sysToken") String sysToken, EnterprisesUsersInfoDTO enterprisesUsersInfoDTO) {
		enterprisesUsersFeiginClient.add(sysToken, enterprisesUsersInfoDTO);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}
}
