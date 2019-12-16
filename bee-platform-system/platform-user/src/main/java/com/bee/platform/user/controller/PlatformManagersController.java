package com.bee.platform.user.controller;


import com.bee.platform.common.dto.PlatformManagersDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.dto.AdminDTO;
import com.bee.platform.user.entity.PlatformManagers;
import com.bee.platform.user.service.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-14
 */
@RestController
@CrossOrigin(origins = "*")
@Api(value = "后台管理——相关接口", tags = "后台管理——相关接口")
@RequestMapping("/manager")
public class PlatformManagersController {

    @Autowired
    private PlatformManagersService platformManagersService;
    @Autowired
    private EnterprisesService enterprisesService;
//    @Autowired
//    private EnterprisesCheckService enterprisesCheckService;
//    @Autowired
//    private ManageUserService manageUserService;



    /*---------------------------------------------------------------------------审核企业相关列表-----------------------------------------------------------------------------*/

//    /**---------------------------------------6---复用，新增审核原因------------------------------**/
//    @ApiOperation(value = "********审核企业申请(0-代表拒绝，1-代表通过)", notes = "审核企业申请")
//    @RequestMapping(value = "/review/{id}", method = RequestMethod.POST)
//    public ResponseResult reviewEnterprise(HttpServletRequest request, @PathVariable("id") int id, @RequestParam int type, @RequestParam(name="reason",required = false) String reason) {
//        ManagerInfo managerInfo = manageUserService.getManagerInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
//        //判断申请类型
//        if (Status.getStatus(type)==null) {
//            log.error("审核类型错误");
//            return ResponseResult.buildResponseResult(ResCodeEnum.AUDIT_TYPE_ERROR);
//        }
//        return enterprisesCheckService.review(id, type, reason, managerInfo.getUsername());
//    }


//    @ApiOperation(value = "审核企业修改申请", notes = "审核企业修改申请")
//    @ApiImplicitParam(name = "id", value = "企业审核表id", required = true, dataType = "int", paramType = "path")
//    @RequestMapping(value = "/reviewUpdate/{id}", method = RequestMethod.POST, consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
//    public ResponseResult reviewUpdate(HttpServletRequest request, @PathVariable("id") int id, @RequestParam int type) {
//        ResponseResult result;
//        ManagerInfo managerInfo = manageUserService.getManagerInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
//        //判断审核信息
//        if (type==0 || type==1 || type==2) {
//            result = enterprisesCheckService.reviewUpdate(id, type, managerInfo.getUsername());
//        } else {
//            log.error("审核信息为空");
//            result = ResponseResult.buildResponseResult(ResCodeEnum.AUDIT_COMMENT_ERROR);
//        }
//        return result;
//    }

//    @ApiOperation(value = "获取全部企业审核列表")
//    @RequestMapping(value = "/checks", method = RequestMethod.GET)
//    public ResponseResult getAllEnterpriseCheck() {
//        List<EnterprisesCheck> enterprisesChecks = enterprisesCheckService.getAllEnterpriseCheck();
//        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterprisesChecks);
//    }

//    @ApiOperation(value = "获取全部企业审核列表（带分页显示）")
//    @RequestMapping(value = "/pageChecks", method = RequestMethod.GET)
//    public ResponseResult< Map<String,Object>> getAllEnterpriseCheck(@RequestParam(value = "page", defaultValue = "0") Integer page,
//                                                @RequestParam(value = "size", defaultValue = "5") Integer size) {
//        Pagination pagination = PageUtils.transFromPage(new Page(size, page));
//        return enterprisesCheckService.getAllEnterpriseCheckByPage(pagination);
//    }

//    @ApiOperation(value = "获取未审核的企业审核列表")
//    @RequestMapping(value = "/untreated_checks", method = RequestMethod.GET)
//    public ResponseResult getUntreatedEnterpriseCheck() {
//        List<EnterprisesCheck> enterprisesChecks = enterprisesCheckService.getUntreatedEnterpriseCheck();
//       return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterprisesChecks);
//    }


//    /**------------------------------------------1-新增-----------------------------**/
//    @ApiOperation(value = "******获取未处理的企业数量（我的待办数量）、已通过企业数量（已通过企业）、申请企业总数（申请总数）")
//    @RequestMapping(value = "/counts", method = RequestMethod.GET)
//    public ResponseResult getCounts() {
//        EnterpriseCheckStatusDTO enterpriseCheckStatusDTO = enterprisesCheckService.getCounts();
//        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterpriseCheckStatusDTO);
//    }

//    /**--------------------------------------------2-新增---------------------------**/
//    @ApiOperation(value = "******获取未处理的企业审核列表（带分页）")
//    @RequestMapping(value = "/unhandled_checks", method = RequestMethod.GET)
//    public ResponseResult<Map<String,Object>> getUntreatedEnterpriseCheck(@RequestParam(value = "page", defaultValue = "0") Integer page,
//                                                                          @RequestParam(value = "size", defaultValue = "5") Integer size, @RequestParam String companyName) {
//        Pagination pagination = PageUtils.transFromPage(new Page(size, page));
//        return enterprisesCheckService.getUntreatedEnterpriseCheck(companyName, pagination);
//    }

//    /**-----------------------------------------3--新增------------------------------**/
//    @ApiOperation(value = "*******获取审核已通过的企业列表（带分页）")
//    @RequestMapping(value = "/passed_checks", method = RequestMethod.GET)
//    public ResponseResult<Map<String,Object>> getOkEnterpriseCheck(@RequestParam(value = "page", defaultValue = "0") Integer page,
//                                               @RequestParam(value = "size", defaultValue = "5") Integer size, @RequestParam String companyName) {
//        Pagination pagination = PageUtils.transFromPage(new Page(size, page));
//        return enterprisesCheckService.getOkEnterpriseCheck(companyName, pagination);
//    }

//    /**-----------------------------------------4--新增---------------------------------**/
//    @ApiOperation(value = "********获取审核未通过企业审核列表（带分页）")
//    @RequestMapping(value = "/refused_checks", method = RequestMethod.GET)
//    public ResponseResult<Map<String,Object>> getNoEnterpriseCheck(@RequestParam(value = "page", defaultValue = "0") Integer page,
//                                               @RequestParam(value = "size", defaultValue = "5") Integer size, @RequestParam String companyName) {
//        Pagination pagination = PageUtils.transFromPage(new Page(size, page));
//        return enterprisesCheckService.getNoEnterpriseCheck(companyName, pagination);
//    }

//    /**---------------------------------------5--新增，以前查看的是返回全部，现在返回根据id-----------------------------**/
//    @ApiOperation(value = "*********获取企业信息(查看详情)")
//    @RequestMapping(value = "/getInfo", method = RequestMethod.GET)
//    public ResponseResult getEnterpriseInfoById(@RequestParam Integer EnterpriseId) {
//        EnterprisesCheck enterprisesCheck = enterprisesCheckService.getEnterpriseAllInfo(EnterpriseId);
//        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterprisesCheck);
//    }


//    /**-------------------------------------7--新增，保存修改后的企业信息-------------------------------**/
//    @ApiOperation(value = "********保存修改后的企业信息")
//    @RequestMapping(value = "/saveInfo", method = RequestMethod.POST)
//    public ResponseResult saveEnterpriseInfo(@RequestBody EnterpriseInfoRQ enterpriseInfoRQ) {
//        return enterprisesCheckService.updateEnterpriseInfo(enterpriseInfoRQ);
//    }

//    @ApiOperation(value = "获取已处理的企业审核列表")
//    @RequestMapping(value = "/treated_checks", method = RequestMethod.GET)
//    public ResponseResult getTreatedEnterpriseCheck() {
//        List<EnterprisesCheck> enterprisesChecks = enterprisesCheckService.getTreatedEnterpriseCheck();
//        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterprisesChecks);
//    }

    @ApiOperation(value = "更新密码")
    @RequestMapping(value = "/update_password", method = RequestMethod.POST)
    public ResponseResult updatePassword(HttpServletRequest request, @RequestParam String old_password, @RequestParam String password) {
        return platformManagersService.updatePassword(request, old_password, password);
    }

    @ApiOperation(value = "更新密码")
    @RequestMapping(value = "/update_password/{id}", method = RequestMethod.POST)
    public ResponseResult updatePassword(@PathVariable int id, @RequestParam String password) {
        return platformManagersService.updatePassword(id, password);
    }

    @ApiOperation(value = "获取管理员")
    @RequestMapping(value = "/get_admins", method = RequestMethod.GET)
    public ResponseResult<List<AdminDTO>> getAdmins() {
        List<AdminDTO> data = enterprisesService.getAdmins(null);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, data);
    }

    @ApiOperation(value = "添加管理员")
    @RequestMapping(value = "/add_admin", method = RequestMethod.POST)
    public ResponseResult addAdmin(@RequestParam String nickname, @RequestParam String username, @RequestParam String password) {
        return platformManagersService.addAdmin(nickname, username, password);
    }

    @ApiOperation(value = "删除管理员")
    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public ResponseResult deleteAdmin(HttpServletRequest request, @PathVariable int id) {
        return platformManagersService.deleteManager(request, id);
    }

    @ApiOperation(value = "获取管理员名字")
    @RequestMapping(value = "/name", method = RequestMethod.GET)
    public ResponseResult getName(HttpServletRequest request) {
        return platformManagersService.getName(request);
    }

    @ApiOperation(value = "获取管理员名字")
    @RequestMapping(value = "/getManageByName", method = RequestMethod.GET)
    public ResponseResult<PlatformManagersDTO> getManagerByName(@RequestParam String userName) {
        PlatformManagers platformManagers = platformManagersService.getManagerByName(userName);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, BeanUtils.copyProperties(platformManagers,PlatformManagersDTO.class));
    }

}

