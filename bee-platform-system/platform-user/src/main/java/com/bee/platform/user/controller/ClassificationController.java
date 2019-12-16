package com.bee.platform.user.controller;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.ContentClassification;
import com.bee.platform.user.entity.SecondaryClassification;
import com.bee.platform.user.service.ClassificationService;
import com.bee.platform.user.service.ManageUserService;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @ClassName: ClassificationController
 * @Description: 系统帮助 分类 controller
 * @Author: fei.sun
 * @Date: 2019/4/28 10:23
 * @Version: 1.0
 */
@Api(value = "系统帮助相关API",tags = "系统帮助相关API")
@CrossOrigin(origins = "*")
@RestController
@RequestMapping("/systemhelper")
@Slf4j
public class ClassificationController {

    @Autowired
    private ClassificationService classificationService;

    /*@Autowired
    private ManageUserService manageUserService;*/
    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation("删除大类")
    @GetMapping("/classify/primary/delete")
    public ResponseResult deletePrimaryClassify(@RequestHeader("sysToken") String sysToken,@RequestParam("id") @ApiParam(name="id",value="大类id",required=true) Integer id){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/

        /*if (ObjectUtils.isEmpty(managerInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.deletePrimaryClassify(userInfo.getId(),id);
    }

    @ApiOperation("查询系统帮助下的大类列表")
    @GetMapping("/classify/primary/query_list")
    public ResponseResult<List<PrimaryClassifyListDTO>> queryPrimaryClassifyList(@RequestParam(value = "id") @ApiParam(value="大类类型id,0 代表首页大类，1 代表常见问题大类，2代表用户指南大类",name = "id",required=true) Integer classifyType){
        return  classificationService.queryPrimaryClassifyList(classifyType);
    }

    /**
     * <!------------帮助首页所有接口-------------->
     * @param
     * @return
     */
    @ApiOperation("帮助首页大类编辑页面查询")
    @GetMapping("/classify/primary/helper/query_list")
    public ResponseResult<HelperSubDTO> queryHelperClassifyInfo(@RequestParam("id") @ApiParam(name="id",value="大类id",required=true) Integer id){
        return classificationService.queryHelperClassifyInfo(id);
    }

    @ApiOperation("帮助首页大类编辑添加小类下拉框")
    @GetMapping("/classify/primary/helper/query_second")
    public ResponseResult<List<SecondaryClassification>> queryHelperSecondClassify(){
        return classificationService.queryHelperSecondClassify();
    }

    @ApiOperation("帮助首页大类编辑添加小类内容下拉框")
    @GetMapping("/classify/primary/helper/query_content")
    public ResponseResult<List<ContentClassification>> queryHelperContent(@RequestParam("id") @ApiParam(name = "id",value = "小类id",required = true) Integer id){
        return classificationService.queryHelperContent(id);
    }

    @ApiOperation("帮助首页添加小类")
    @GetMapping("/classify/second/helper/submit")
    public ResponseResult submitHelperSecondClassify(@RequestHeader("sysToken") String sysToken,@ApiParam(name = "pId",value = "大类id",required = true) @RequestParam("pId") Integer pId
            ,@ApiParam(name = "sId",value = "小类id",required = true) @RequestParam("sId") Integer sId
            ,@ApiParam(name = "cId",value = "文章id",required = true) @RequestParam("cId") Integer cId
            ,@ApiParam(name = "weights",value = "文章显示排序",required = true) @RequestParam("weights") Integer weights){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.submitHelperSecondClassify(userInfo.getId(),pId,sId,cId,weights);
    }

    @ApiOperation("帮助首页删除小类")
    @PostMapping("/classify/second/helper/delete")
    public ResponseResult deleteHelperSecondaryClassify(@RequestHeader("sysToken") String sysToken,@ApiParam(name = "id",value = "帮助首页下列表标识id",required = true) @RequestParam("id") Integer id){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.deleteHelperSecondaryClassify(userInfo.getId(),id);
    }

    @ApiOperation("常见问题或用户指南大类编辑列表查询")
    @GetMapping("/classify/primary/edit/query_list")
    public ResponseResult<EditClassifyDTO> queryEditClassifyInfo(@RequestParam("id") @ApiParam(name="id",value="常见问题或用户指南大类id",required=true) Integer id){
        return classificationService.queryEditClassifyInfo(id);
    }

    @ApiOperation("常见问题或用户指南大类编辑列表提交")
    @GetMapping("/classify/primary/edit/submit")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "大类id",name = "id"),
            @ApiImplicitParam(value = "大类名称",name = "name",required = true),
            @ApiImplicitParam(value = "大类显示排序",name = "weights",required = true),
            @ApiImplicitParam(value = "大类类型标识",name = "classifyType",required = true)
    })
    public ResponseResult submitPrimaryClassify(@RequestHeader("sysToken") String sysToken,@RequestParam(value = "id",required = false) Integer id
            ,@RequestParam(value = "name") String name
            ,@RequestParam(value = "weights") Integer weights
            ,@RequestParam(value = "classifyType") Integer classifyType){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.submitPrimaryClassify(userInfo.getId(),id,name,weights,classifyType);
    }

    @ApiOperation("编辑小类列表查询")
    @GetMapping("/classify/second/edit/query_list")
    public ResponseResult<SecondClassifyListDTO> querySecondClassifyInfo(@ApiParam(name = "id",value = "小类id",required = true) @RequestParam Integer id){
        return classificationService.querySecondClassifyInfo(id);
    }

    @ApiOperation("常见问题或用户指南下编辑或者添加小类提交")
    @PostMapping("/classify/second/edit/submit")
    public ResponseResult updateOrSaveSecondClassify(@RequestHeader("sysToken") String sysToken,@ApiParam(value = "小类id",name = "id") @RequestParam(value = "id",required = false) Integer id
            ,@ApiParam(value = "小类名称",name = "name",required = true) @RequestParam("name") String name
            ,@ApiParam(value = "小类显示排序",name = "weights",required = true) @RequestParam("weights") Integer weights
            ,@ApiParam(value = "大类id",name = "pId",required = true) @RequestParam("pId") Integer pId
            ,@ApiParam(value = "大类类型标识",name = "classifyType",required = true) @RequestParam("classifyType") Integer classifyType){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.updateOrSaveSecondClassify(userInfo.getId(),id,name,weights,classifyType,pId);
    }

    @ApiOperation("删除小类接口")
    @GetMapping("/classify/second/delete")
    public ResponseResult deleteSecondClassify(@RequestHeader("sysToken") String sysToken,@RequestParam(value = "id") @ApiParam(name = "id",value = "小类id",required = true) Integer id){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.deleteSecondClassify(userInfo.getId(),id);
    }

    @ApiOperation("添加或者更新文章")
    @PostMapping("/classify/content/edit/submit")
    public ResponseResult saveOrUpdateContent(@RequestHeader("sysToken") String sysToken,@RequestBody ContentDTO contentDTO){
        if(null==contentDTO){
            log.error("提交文章内容的参数为空！");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        try {
            /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
            AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
            if (ObjectUtils.isEmpty(userInfo)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            classificationService.saveOrUpdateContent(userInfo.getId(),contentDTO);
        } catch (Exception e) {
            log.error("保存文章内容出错 ： ",e);
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation("编辑文章内容查询")
    @GetMapping("/classify/content/edit/query")
    public ResponseResult<ContentDTO> queryContentClassification(@RequestParam(value = "id") @ApiParam(name = "id",value = "文章id",required = true) Integer id){
        return classificationService.queryContentClassification(id);
    }

    @ApiOperation("删除文章内容")
    @GetMapping("/classify/content/delete")
    public ResponseResult deleteContent(@RequestHeader("sysToken") String sysToken,@RequestParam(value = "id",required = false) @ApiParam(name = "id",value = "文章id") Integer id){
        try {
            if(id !=null){
                /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
                AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
                if (ObjectUtils.isEmpty(userInfo)) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
                }
                classificationService.deleteContent(userInfo.getId(),id);
            }
        } catch (Exception e) {
            log.error("删除文章出错 ： ",e);
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation("联系客服电话邮箱查询")
    @GetMapping("/helper/customer_service/query")
    public ResponseResult<CustomerServiceDTO> queryCustomerServiceInfo(){
        return classificationService.queryCustomerServiceInfo();
    }

    @ApiOperation("修改客服电话邮箱")
    @GetMapping("/helper/customer_service/update")
    public ResponseResult updateCustomerServiceInfo(@RequestHeader("sysToken") String sysToken,
                                                    @ApiParam(name = "hotLine",value = "客服热线",required = true) @RequestParam String hotLine,
                                                    @ApiParam(name = "eMail",value = "邮箱地址",required = true) @RequestParam String eMail){
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return classificationService.updateCustomerServiceInfo(userInfo.getId(),hotLine,eMail);
    }
}
