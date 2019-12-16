package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckAmendRQ;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDTO;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDetailsDTO;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckRQ;
import com.bee.platform.user.entity.EnterprisesRelationUserCheck;
import com.bee.platform.user.rq.EnterprisesRelationUserRQ;
import com.bee.platform.user.service.EnterprisesRelationUserCheckService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 * 企业关联用户审核表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */


@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "企业关联用户相关接口",tags = "企业关联用户相关接口")
@RestController
@RequestMapping("/enterprisesRelationUserCheck")
public class EnterprisesRelationUserCheckController {
    @Autowired
    private EnterprisesRelationUserCheckService enterprisesRelationUserCheckService;
    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "企业关联用户",notes = "企业关联用户")
    @PostMapping("/enterprisesRelationUser")
    public ResponseResult enterprisesRelationUser(HttpServletRequest request, @RequestBody @Valid EnterprisesRelationUserRQ rq){
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getNickname())
                ||ObjectUtils.isEmpty(userInfo.getPhone())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterprisesRelationUserCheckService.enterprisesRelationUser(userInfo,rq);
    }

    @ApiOperation(value = "根据手机号码和姓名申请时间查询企业申请列表",notes = "根据手机号码和姓名申请时间查询企业申请列表")
    @PostMapping("/getEnterprisesRelationUserCheckApplyList")
    public ResponseResult<List<EnterprisesRelationUserCheckDTO>> getEnterprisesRelationUserCheckApplyList(HttpServletRequest request,@RequestBody EnterprisesRelationUserCheckRQ rq, Page page) {
    	AuthPlatformUserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
    	if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
    	Pagination pagination = PageUtils.transFromPage(page);
    	Integer enterpriseId = userInfo.getOrgId();


    	List<EnterprisesRelationUserCheckDTO> list = enterprisesRelationUserCheckService.getApplyList(rq,enterpriseId,pagination);
    	if (CollectionUtils.isEmpty(list)){
            ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    	return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }

    @ApiOperation(value = "根据ID修改企业关联审核",notes = "根据ID修改企业关联审核")
    @PostMapping("/upDateEnterprisesRelationUserCheck")
    public ResponseResult<EnterprisesRelationUserCheck> upDateEnterprisesRelationUserCheck(HttpServletRequest request, @RequestBody EnterprisesRelationUserCheckAmendRQ rq) {
    	AuthPlatformUserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
    	return enterprisesRelationUserCheckService.enterprisesRelationUserCheckUpDate(rq, userInfo);
    }


    @ApiOperation(value = "企业关联名称校验")
    @GetMapping("/enterpriseRelationCheck")
    public ResponseResult enterpriseRelationCheck(HttpServletRequest request,@RequestParam() String name){
        if(StringUtils.isEmpty(name)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return enterprisesRelationUserCheckService.enterpriseRelationCheck(request,name);

    }


    @ApiOperation(value = "关联审核详情页",notes = "关联审核详情页")
    @PostMapping("/enterprisesRelationUserCheckDetails")
    public ResponseResult<EnterprisesRelationUserCheckDetailsDTO> enterprisesRelationUserCheckDetails(HttpServletRequest request, @RequestParam Integer id) {
    	AuthPlatformUserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
    	return enterprisesRelationUserCheckService.getEnterprisesRelationUserCheckDetails(id,userInfo);
    }
    
//    @ApiOperation(value = "模糊查询企业审核列表",notes = "模糊查询企业审核列表")
//    @PostMapping("/getEnterprisesRelationUserCheck")
//    public ResponseResult<ResponseResult<ArrayList<EnterprisesRelationUserCheckDTO>>> getEnterprisesRelationUserCheck(HttpServletRequest request,@RequestBody Page page) {
//    	UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
//		if (ObjectUtils.isEmpty(userInfo)) {
//			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
//		}
//		Pagination pagination = PageUtils.transFromPage(page);
//		ResponseResult<ArrayList<EnterprisesRelationUserCheckDTO>> list = enterprisesRelationUserCheckService.getAllApplyList(userInfo.getOrgId(),pagination);
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
//    }
}

