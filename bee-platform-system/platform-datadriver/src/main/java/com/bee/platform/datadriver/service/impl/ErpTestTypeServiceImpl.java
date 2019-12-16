package com.bee.platform.datadriver.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpTestReportMapper;
import com.bee.platform.datadriver.dao.mapper.ErpTestTypeMapper;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.bee.platform.datadriver.entity.ErpTestType;
import com.bee.platform.datadriver.rq.TestReportTypeRQ;
import com.bee.platform.datadriver.service.ErpTestTypeService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 化验类型 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpTestTypeServiceImpl extends ServiceImpl<ErpTestTypeMapper, ErpTestType> implements ErpTestTypeService {

    @Autowired
    private ErpTestTypeMapper testTypeMapper;
    
//    @Autowired
//    private ErpCodeMepper erpCodeMepper;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private ErpTestReportMapper testReportMapper;
    
    private static Integer ZERO = 0;

    /**
     * 分页查询化验类型列表
     * @param pagination
     * @return
     */
    @Override
    public List<ErpTestType> listErpTestType(String sysToken, Integer companyId, TestReportTypeRQ rq,Pagination pagination) {
        List<AuthEnterpriseFlatDTO> enterprises;
        if (ObjectUtils.isEmpty(companyId)) {
            enterprises = enterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
        } else {
            enterprises = enterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        }
        if (org.springframework.util.CollectionUtils.isEmpty(enterprises)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpTestReportServiceImpl", "listTestReport");
            return Lists.newArrayList();
        }
        
        List<Integer> enterpriseIds = enterprises.stream().map(a -> a.getValue()).collect(Collectors.toList());
        Wrapper wrapper = new EntityWrapper<ErpTestType>().eq("deleted", Status.FALSE.getKey());
        if (!ObjectUtils.isEmpty(rq)){
            if (!StringUtils.isEmpty(rq.getType())){
                wrapper.and().eq("type",rq.getType());
            }
            if (rq.getStatus() != null){
                wrapper.and().eq("status",rq.getStatus());
            }
            if (!StringUtils.isEmpty(rq.getName())){
                wrapper.like("name",rq.getName());
            }
        }
        if (!org.springframework.util.CollectionUtils.isEmpty(enterpriseIds)){
            wrapper.in("enterprise_id", enterpriseIds);
        }
        wrapper.orderBy("create_time",false);
        return testTypeMapper.selectPage(pagination,wrapper);
    }

    /**
     * 删除化验类型
     * @param id
     * @return
     */
    @Override
    public ResponseResult<Integer> deleteTestType(AuthPlatformUserInfo userInfo, String id) {
        ErpTestType erpTestType = testTypeMapper.selectById(id);
        if (testReportMapper.selectCount(new EntityWrapper<>(new ErpTestReport()
                .setTestType(erpTestType.getId())
                .setDeleted(Status.FALSE.getKey()))) > ZERO){
            log.error("删除化验单失败,调用{}类{}方法出错","ErpTestTypeServiceImpl","deleteTestType()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.USED_TEST_TYPE_CANNOT_DELETE);
        }
        if (testTypeMapper.updateById(new ErpTestType()
                .setId(Integer.valueOf(id)).setDeleted(Status.TRUE.getKey())) < ZERO){
            log.error("删除化验单失败,调用{}类{}方法出错","ErpTestTypeServiceImpl","deleteTestType()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_TEST_TYPE_DELETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,Integer.valueOf(id));
    }

    /**
     * 保存化验单类型
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<Integer> saveTestType(AuthPlatformUserInfo userInfo, TestReportTypeRQ rq) {
        ErpTestType testType = new ErpTestType();
        BeanUtils.copyProperties(rq,testType);
        if (testType.getId() == null){
            if (testTypeMapper.insert(testType.setDeleted(Status.FALSE.getKey())
                    .setEnterpriseId(userInfo.getOrgId())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("新增化验类型失败,调用{}类{}方法出错","ErpTestTypeServiceImpl","saveTestType()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_TEST_TYPE_ADD_FAILED);
            }
        }else {
            if (testTypeMapper.updateById(testType) <= ZERO){
                log.error("修改化验类型失败,调用{}类{}方法出错","ErpTestTypeServiceImpl","saveTestType()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_TEST_TYPE_UPDATE_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,testType.getId());
    }

    /**
    *	根据企业id查询类型
    * @return
    */
   @Override
   public ResponseResult<List<ErpTestType>> getTestType(Integer userId,String sysToken) {
       List<AuthEnterpriseFlatDTO> enterprises = enterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
       List<ErpTestType> list = new ArrayList<>();
       if (!ObjectUtils.isEmpty(enterprises)){
           list = testTypeMapper.selectList(new EntityWrapper<ErpTestType>()
                   .eq("deleted",Status.FALSE.getKey()).and()
                   .eq("status",Status.TRUE.getKey())
                   .in("enterprise_id",enterprises.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList())));
       }
	   return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
   }
}
