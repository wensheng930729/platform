package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.AuditStateEnum;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.rq.EnterpriseRegisterInfoRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.EnterpriseCheckDTO;
import com.bee.platform.user.dto.EnterpriseCheckDetailDTO;
import com.bee.platform.user.dto.EnterpriseWithAttacheDTO;
import com.bee.platform.user.entity.EnterprisesCheck;
import com.bee.platform.user.rq.EnterprisesCheckAuditRQ;
import com.bee.platform.user.service.EnterprisesCheckService;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "enterprisesCheck", tags = "企业审核相关接口")
@RequestMapping("/enterprisesCheck")
public class EnterprisesCheckController {

    @Autowired
    private EnterprisesCheckService enterprisesCheckService;
    @Autowired
    private UsersService usersService;
    @Autowired
    private ManageUserService manageUserService;
    
    @Autowired
    private AuthPlatformUserService authPlatformUserService;


    @ApiOperation(value = "查询用户申请认证还未通过的企业信息")
    @RequestMapping(value = "/authenticatedList", method = RequestMethod.GET)
    public ResponseResult<List<EnterprisesCheck>> getAuthenticatedList(@RequestParam String phone) {
        if (StringUtils.isEmpty(phone)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesCheckService.getAuthenticatedList(phone));
    }

//    @ApiOperation(value = "通过企业审核表id撤回认证记录")
//    @RequestMapping(value = "/revoke", method = RequestMethod.POST)
//    public ResponseResult<String> revokeAuthentication(@RequestParam Integer checkId) {
//        if (ObjectUtils.isEmpty(checkId)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
//        }
//        return enterprisesCheckService.revokeAuthentication(checkId);
//    }

//    @ApiOperation(value = "通过企业审核表id再次认证")
//    @RequestMapping(value = "/reAuthentication", method = RequestMethod.POST)
//    public ResponseResult<String> reAuthentication(@RequestParam Integer checkId) {
//        if (ObjectUtils.isEmpty(checkId)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
//        }
//        return enterprisesCheckService.reAuthentication(checkId);
//    }

    @ApiOperation(value = "通过企业审核表id查询企业审核详细信息")
    @RequestMapping(value = "/enterpriseCheckInfo", method = RequestMethod.GET)
    public ResponseResult<EnterpriseCheckDTO> getEnterpriseCheckInfo(@RequestParam Integer checkId) {
        if (ObjectUtils.isEmpty(checkId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesCheckService.getEnterpriseCheckInfo(checkId));
    }

    @ApiOperation(value = "用户查看申请注册企业记录")
    @GetMapping(value = "/getUserApplyEnterpriseList")
    public ResponseResult getUserApplyList(HttpServletRequest request,Page page){
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        
        return enterprisesCheckService.getUserApplyList(cvtAuthUserToUserInfo(userInfo), page);
        
    }

    @ApiOperation(value = "查看企业申请详情")
    @GetMapping(value = "/getEnterpriseApplyDetail")
    public ResponseResult getEnterpriseApplyDetail(@RequestParam int id){
        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesCheckService.getEnterpriseApplyDetail(id));
    }

    @ApiOperation(value = "企业管理-查看详情----后台接口")
    @GetMapping(value = "/Info")
    public ResponseResult<EnterpriseCheckDetailDTO> getCheckInfo(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesCheckService.getCheckInfo(id));
    }
    
    @ApiOperation(value = "企业管理-审核企业----后台接口")
    @PostMapping(value = "/audit")
    public ResponseResult audit(@Valid @RequestBody EnterprisesCheckAuditRQ rq, HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        //判断申请类型
        if (!(AuditStateEnum.NOT_PASSED.getKey().equals(rq.getType()) || 
        		AuditStateEnum.PASSED.getKey().equals(rq.getType()))) {
        	return ResponseResult.buildResponseResult(ResCodeEnum.AUDIT_TYPE_ERROR);
        }
        return enterprisesCheckService.audit(rq.getId(), rq.getType(), rq.getReason(), userInfo);
    }
    
   /* @ApiOperation(value = "变更企业管理员-----后台接口")
    @PostMapping(value = "/modifyAdmin")
    public ResponseResult modifyAdmin(@Valid @RequestBody EnterprisesModifyAdminRQ rq, @RequestHeader(value = "sysToken") String sysToken) {
    	ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if (ObjectUtils.isEmpty(managerInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
    	return enterprisesCheckService.modifyAdmin(rq.getCheckId(), rq.getUserId(), managerInfo);
    }*/

    @ApiOperation(value = "企业管理 获取企业信息")
    @PostMapping("/getCompanyInfo")
    public ResponseResult<EnterpriseWithAttacheDTO> getCompanyInfo(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterprisesCheckService.getCompanyInfo(userInfo);
    }

    @ApiOperation(value = "企业管理 修改企业信息")
    @PostMapping("/updateCompanyInfo")
    public ResponseResult updateCompanyInfo(HttpServletRequest request, @RequestBody() @Valid EnterpriseRegisterInfoRQ enterpriseRegisterInfoRQ) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterprisesCheckService.updateCompanyInfo(userInfo,enterpriseRegisterInfoRQ);
    }

    @ApiOperation(value = "获取全部企业审核信息列表（分页）")
    @RequestMapping(value = "/pageEterpriseChecks", method = RequestMethod.GET)
    public ResponseResult<Map<String,Object>> getAllEnterpriseCheck(@RequestParam(value = "page", defaultValue = "0") Integer page,
                                                                    @RequestParam(value = "size", defaultValue = "5") Integer size) {
        Pagination pagination = PageUtils.transFromPage(new Page(size, page));
        return enterprisesCheckService.getAllEnterpriseCheckByPage(pagination);
    }
    
    private AuthPlatformUserInfo cvtAuthUserToUserInfo(AuthPlatformUserInfo authUser) {
        AuthPlatformUserInfo user = new AuthPlatformUserInfo();
        BeanUtils.copyProperties(authUser, user);
        return user;
    }

}

