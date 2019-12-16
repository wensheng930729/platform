package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpRepositoryListDTO;
import com.bee.platform.datadriver.rq.ErpRepositoryAddRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryDeleteRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryEnableRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryUpdataRQ;
import com.bee.platform.datadriver.service.ErpRepositoryService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 仓库档案 前端控制器
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@Slf4j
@RestController
@RequestMapping("/erpRepository")
@Api(value = "仓库档案相关接口", tags = "仓库档案相关接口")
public class ErpRepositoryController {

	@Autowired
	private AuthUserFeignClient userInfoFeignClient;
	@Autowired
	private ErpRepositoryService erpRepositoryService;
	@Autowired
	private UserInfoUtils userInfoUtils;


	@PostMapping("/query")
    @ApiOperation(value = "查询仓库档案")
    public ResponseResult query(HttpServletRequest request, Page page) {
        //AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        Pagination pagination = PageUtils.transFromPage(page);
        if (Objects.isNull(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
		return erpRepositoryService.query(pagination,companyId);
	}
	
	@PostMapping("/updateErpRepositoryEnable")
    @ApiOperation(value = "启用禁用仓库档案")
    public ResponseResult updateErpRepositoryEnable(@RequestHeader("sysToken") String sysToken,@RequestBody ErpRepositoryEnableRQ enableRQ) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpRepositoryService.updateErpRepositoryEnable(userInfo, enableRQ));
		
	}

	@PostMapping("/deleteErpRepositoryEnable")
    @ApiOperation(value = "删除仓库档案")
    public ResponseResult deleteErpRepositoryEnable(@RequestHeader("sysToken") String sysToken,@RequestBody ErpRepositoryDeleteRQ deleteRQ) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return erpRepositoryService.delete(userInfo, deleteRQ);
	}
	
	
	@PostMapping("/add")
    @ApiOperation(value = "添加仓库档案")
    public ResponseResult add(@RequestHeader("sysToken") String sysToken,@RequestBody ErpRepositoryAddRQ addRQ) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return erpRepositoryService.add(userInfo, addRQ);
	}

	@GetMapping("/get")
    @ApiOperation(value = "需要添加仓库档案")
    public ResponseResult get(@RequestParam int id) {
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpRepositoryService.get(id));
	}
	
	
	@PostMapping("/update")
    @ApiOperation(value = "编辑添加仓库档案")
    public ResponseResult update(@RequestHeader("sysToken") String sysToken,@RequestBody ErpRepositoryUpdataRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return erpRepositoryService.update(userInfo, rq);
	}

	@ApiOperation(value = "查询当前登录用户及其子企业的仓库信息")
	@GetMapping("/getRepositoryList")
	public ResponseResult getRepositoryList(HttpServletRequest request){

		AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
		if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getOrgId())) {
			log.error("无法获取用户信息");
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		String sysToken = request.getHeader("sysToken");
		List<ErpRepositoryListDTO> dto = erpRepositoryService.getRepositoryList(userInfo,sysToken);

		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);

	}

	@PostMapping("/getStatus")
    @ApiOperation(value = "根据状态查询仓库档案")
    public ResponseResult getStatus(HttpServletRequest request, Page page,Integer status) {
		Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        Pagination pagination = PageUtils.transFromPage(page);
        if (Objects.isNull(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
		return erpRepositoryService.getStatus(pagination, companyId, status);
	}
}

