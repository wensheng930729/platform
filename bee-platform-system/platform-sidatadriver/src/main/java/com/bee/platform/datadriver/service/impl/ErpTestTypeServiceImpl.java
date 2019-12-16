package com.bee.platform.datadriver.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import com.bee.platform.datadriver.dto.ErpTestTypeDTO;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
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
    @Autowired
    private ErpTestReportMapper testReportMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    private static Integer ZERO = 0;

    /**
     * 分页查询化验类型列表
     * @param pagination
     * @return
     */
    @Override
    public List<ErpTestTypeDTO> listErpTestType(AuthPlatformUserInfo userInfo, TestReportTypeRQ rq, Pagination pagination) {
        rq.setEnterpriseId(userInfo.getOrgId());
        List<ErpTestTypeDTO> list = testTypeMapper.listErpTestType(rq,pagination);
        //查询企业名称
        if (!CollectionUtils.isEmpty(list)) {
            List<Integer> enterpriseIds = new ArrayList<>(list.size());
            list.forEach(testType -> {
                if (Objects.nonNull(testType.getEnterpriseId())) {
                    enterpriseIds.add(testType.getEnterpriseId());
                }
            });
            if (!CollectionUtils.isEmpty(enterpriseIds)) {
                List<AuthEnterpriseFeignDetailDTO> enterprises = authEnterpriseFeignClient
                        .getEnterpriseMoreDetail(enterpriseIds).getObject();
                if (!CollectionUtils.isEmpty(enterprises)) {
                    Map<Integer, String> enterpriseMap = enterprises.stream().collect(Collectors
                            .toMap(AuthEnterpriseFeignDetailDTO::getId, AuthEnterpriseFeignDetailDTO::getName));
                    list.forEach(testType -> {
                        testType.setEnterpriseName(enterpriseMap.get(testType.getEnterpriseId()));
                    });
                }
            }
        }
        return list.stream().map(temp -> {
            if (ZERO.equals(temp.getType())) {
                temp.setEnterpriseName(userInfo.getOrg_name());
            }
            return temp;
        }).collect(Collectors.toList());
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
   public ResponseResult<List<ErpTestType>> getTestType(Integer orgId) {
       List<ErpTestType> list = testTypeMapper.selectList(new EntityWrapper<ErpTestType>()
               .eq("deleted",Status.FALSE.getKey()).and()
               .eq("status",Status.TRUE.getKey())
               .ne("type",ZERO)
               .eq("enterprise_id",orgId));
       // 期初
       ErpTestType origin = testTypeMapper.selectOne(new ErpTestType().setDeleted(Status.FALSE.getKey()).setType(ZERO));
       if (!ObjectUtils.isEmpty(origin)){
           list.add(origin);
       }
	   return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
   }
}
