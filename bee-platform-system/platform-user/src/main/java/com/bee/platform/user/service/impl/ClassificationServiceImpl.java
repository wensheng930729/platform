package com.bee.platform.user.service.impl;


import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.DateUtil;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.service.ClassificationService;
import com.bee.platform.user.service.ManageUserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @ClassName: ClassificationServiceImpl
 * @Description: 系统帮助 service
 * @Author: fei.sun
 * @Date: 2019/4/28 10:25
 * @Version: 1.0
 */

@Service
@Slf4j
public class ClassificationServiceImpl implements ClassificationService {

    @Autowired
    private PrimaryClassificationMapper primaryClassificationMapper;

    @Autowired
    private SecondaryClassificationMapper secondaryClassificationMapper;

    @Autowired
    private ContentClassificationMapper contentClassificationMapper;

    @Autowired
    private HelperRelationMapper helperRelationMapper;

    @Autowired
    private CustomerServiceInfoMapper customerServiceInfoMapper;

    @Autowired
    private MPlatformManagersMapper managersMapper;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deletePrimaryClassify(Integer userId,Integer id) {
        PrimaryClassification primaryClassification = primaryClassificationMapper.selectById(id);
        primaryClassification.setStatus(1).setUpdateUid((long)userId).setUpdateTime(LocalDateTime.now());
        log.info(" 删除大类 : "+primaryClassification);
        Integer result = primaryClassificationMapper.updateById(primaryClassification);
        if(result>0){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
    }

    @Override
    public ResponseResult queryPrimaryClassifyList(Integer classifyType) {
        List<PrimaryClassifyListDTO> primaryClassifyListDTOS ;
        if(classifyType.equals(0)){
            primaryClassifyListDTOS = primaryClassificationMapper.queryHelperPrimaryClassifyList();
        }else {
            primaryClassifyListDTOS = primaryClassificationMapper.queryPrimaryClassifyList(classifyType);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,primaryClassifyListDTOS);
    }

    @Override
    public ResponseResult<HelperSubDTO> queryHelperClassifyInfo(Integer id) {
        PrimaryClassification primaryClassification = primaryClassificationMapper.selectById(id);
        HelperSubDTO helperSubDTO = new HelperSubDTO();
        BeanUtils.copyProperties(primaryClassification,helperSubDTO);
        LocalDateTime updateTime = primaryClassification.getUpdateTime();
        Long userId = primaryClassification.getUpdateUid();
        if(updateTime == null){
            updateTime = primaryClassification.getCreateTime();
            userId = primaryClassification.getCreateUid();
        }
        if(updateTime!=null&&userId!=null){
            String time = DateUtil.convertDateToString(updateTime);
            PlatformManagers platformManagers = managersMapper.selectById(userId);
            helperSubDTO.setUpdateName(platformManagers.getNickname());
            helperSubDTO.setUpdateTime(time);
        }
        List<HelperContentDTO> helperContentDTOs = primaryClassificationMapper.queryHelperSecondClassifyInfo(id);
        helperSubDTO.setHelperContentDTOS(helperContentDTOs);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,helperSubDTO);
    }

    @Override
    public ResponseResult<List<SecondaryClassification>> queryHelperSecondClassify() {
        List<SecondaryClassification> secondaryClassifications = secondaryClassificationMapper.queryHelperSecondClassify();
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,secondaryClassifications);
    }

    @Override
    public ResponseResult<List<ContentClassification>> queryHelperContent(Integer id) {
        EntityWrapper<ContentClassification> entityWrapper = new EntityWrapper<>();
        entityWrapper.eq("p_id",id);
        entityWrapper.eq("status",0);
        List<ContentClassification> contentClassifications = contentClassificationMapper.selectList(entityWrapper);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,contentClassifications);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteHelperSecondaryClassify(Integer userId,Integer id) {
        Integer result = helperRelationMapper.deleteById(id);
        if(result.equals(0)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<EditClassifyDTO> queryEditClassifyInfo(Integer id) {
        PrimaryClassification primaryClassification = primaryClassificationMapper.selectById(id);
        if(primaryClassification==null){
            log.error("查询大类编辑列表出错，根据id = {} 没有找到数据！",id);
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        EditClassifyDTO editClassifyDTO=new EditClassifyDTO();
        BeanUtils.copyProperties(primaryClassification,editClassifyDTO);
        LocalDateTime updateTime = primaryClassification.getUpdateTime();
        Long userId = primaryClassification.getUpdateUid();
        if(updateTime == null){
            updateTime = primaryClassification.getCreateTime();
            userId = primaryClassification.getCreateUid();
        }
        if(updateTime!=null&&userId!=null){
            String time = DateUtil.convertDateToString(updateTime);
            PlatformManagers platformManagers = managersMapper.selectById(userId);
            editClassifyDTO.setUpdateName(platformManagers.getNickname());
            editClassifyDTO.setUpdateTime(time);
        }
        List<EditSecondClassifyDTO> editSecondClassifyDTOS = primaryClassificationMapper.queryEditClassifyInfo(id);
        editClassifyDTO.setEditSecondClassifyDTOs(editSecondClassifyDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,editClassifyDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult submitPrimaryClassify(Integer userId,Integer id,String name,Integer weights,Integer classifyType) {
        if(null==id){
            //新增
            PrimaryClassification primaryClassification = new PrimaryClassification();
            primaryClassification.setName(name);
            primaryClassification.setWeights(weights);
            primaryClassification.setStatus(0);
            primaryClassification.setClassifyType(classifyType);
            primaryClassification.setCreateTime(LocalDateTime.now()).setCreateUid((long)userId);
            primaryClassificationMapper.insert(primaryClassification);
        }else {
            //更新
            PrimaryClassification primaryClassification = primaryClassificationMapper.selectById(id);
            primaryClassification.setName(name);
            primaryClassification.setWeights(weights);
            primaryClassification.setUpdateTime(LocalDateTime.now()).setUpdateUid((long)userId);
            primaryClassificationMapper.updateById(primaryClassification);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateOrSaveSecondClassify(Integer userId,Integer id, String name, Integer weights,Integer classifyType,Integer pId) {
        if(null==id){
            //新增
            SecondaryClassification sc = new SecondaryClassification();
            sc.setName(name)
              .setWeights(weights)
              .setStatus(0)
              .setPId(pId)
              .setCreateTime(LocalDateTime.now()).setCreateUid((long)userId);
            secondaryClassificationMapper.insert(sc);
        }else {
            //更新
            SecondaryClassification secondaryClassification = secondaryClassificationMapper.selectById(id);
            secondaryClassification.setName(name);
            secondaryClassification.setWeights(weights).setUpdateTime(LocalDateTime.now()).setUpdateUid((long)userId);
            secondaryClassificationMapper.updateById(secondaryClassification);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteSecondClassify(Integer userId,Integer id) {
        SecondaryClassification secondaryClassification = secondaryClassificationMapper.selectById(id);
        secondaryClassification.setStatus(1).setUpdateUid((long)userId).setUpdateTime(LocalDateTime.now());
        secondaryClassificationMapper.updateById(secondaryClassification);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public void saveOrUpdateContent(Integer userId,ContentDTO contentDTO) {
        Integer id = contentDTO.getId();
        if(null==id){
            ContentClassification contentClassification = new ContentClassification();
            BeanUtils.copyProperties(contentDTO,contentClassification);
            contentClassification.setStatus(0);
            contentClassification.setCreateTime(LocalDateTime.now()).setCreateUid((long)userId);
            contentClassificationMapper.insert(contentClassification);
        }else {
            ContentClassification contentClassification = contentClassificationMapper.selectById(id);
            contentClassification.setName(contentDTO.getName());
            contentClassification.setWeights(contentDTO.getWeights());
            contentClassification.setPcContent(contentDTO.getPcContent());
            contentClassification.setMobileContent(contentDTO.getMobileContent())
                    .setUpdateTime(LocalDateTime.now())
                    .setUpdateUid((long)userId);
            contentClassificationMapper.updateById(contentClassification);
        }
    }

    @Override
    public void deleteContent(Integer userId,Integer id) {
        ContentClassification contentClassification = contentClassificationMapper.selectById(id);
        contentClassification.setStatus(1).setUpdateUid((long)userId).setUpdateTime(LocalDateTime.now());
        contentClassificationMapper.updateById(contentClassification);
    }

    @Override
    public ResponseResult<SecondClassifyListDTO> querySecondClassifyInfo(Integer id) {
        SecondaryClassification secondaryClassification = secondaryClassificationMapper.selectById(id);
        if(secondaryClassification==null){
            log.error("查询小类列表 参数 错误 ，根据id = {} 没有查询到值！",id);
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        SecondClassifyListDTO secondClassifyListDTO = new SecondClassifyListDTO();
        secondClassifyListDTO.setId(secondaryClassification.getId());
        secondClassifyListDTO.setName(secondaryClassification.getName());
        secondClassifyListDTO.setWeights(secondaryClassification.getWeights());
        LocalDateTime updateTime = secondaryClassification.getUpdateTime();
        Long userId = secondaryClassification.getUpdateUid();
        if(updateTime == null){
            updateTime = secondaryClassification.getCreateTime();
            userId = secondaryClassification.getCreateUid();
        }
        if(updateTime!=null&&userId!=null){
            String time = DateUtil.convertDateToString(updateTime);
            PlatformManagers platformManagers = managersMapper.selectById(userId);
            secondClassifyListDTO.setUpdateName(platformManagers.getNickname());
            secondClassifyListDTO.setUpdateTime(time);
        }
        EntityWrapper<ContentClassification> entityWrapper = new EntityWrapper<>();
        entityWrapper.eq("p_id",id);
        entityWrapper.eq("status",0);
        List<ContentClassification> contentClassifications = contentClassificationMapper.selectList(entityWrapper);
        List<SecondContentDTO> secondContentDTOS = new ArrayList<>();
        contentClassifications.stream().forEach(contentClassification -> {
            SecondContentDTO secondContentDTO = new SecondContentDTO();
            BeanUtils.copyProperties(contentClassification,secondContentDTO);
            secondContentDTOS.add(secondContentDTO);
        });
        secondClassifyListDTO.setSecondContentDTOs(secondContentDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,secondClassifyListDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult submitHelperSecondClassify(Integer userId,Integer pId,Integer sId,Integer cId,Integer weights) {
        HelperRef ref = new HelperRef();
        ref.setCId(cId)
           .setPId(pId)
           .setSId(sId)
           .setWeights(weights);
        helperRelationMapper.insert(ref);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<CustomerServiceDTO> queryCustomerServiceInfo() {
        List<CustomerServiceDTO> customerServiceDTOS = primaryClassificationMapper.queryCustomerServiceInfo();
        CustomerServiceDTO customerServiceDTO = null;
        if(customerServiceDTOS!=null&&customerServiceDTOS.size()>0){
            customerServiceDTO = customerServiceDTOS.get(0);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,customerServiceDTO);
    }

    @Override
    public ResponseResult updateCustomerServiceInfo(Integer managerId, String hotLine, String eMail) {
        List<CustomerServiceInfo> customerServiceInfos = customerServiceInfoMapper.selectList(null);
        if(customerServiceInfos==null||customerServiceInfos.size()==0){
            CustomerServiceInfo customerServiceInfo = new CustomerServiceInfo();
            customerServiceInfo.setEMail(eMail);
            customerServiceInfo.setCustomerServiceHotline(hotLine);
            customerServiceInfo.setCreateId((long)managerId);
            customerServiceInfo.setCreateTime(LocalDateTime.now());
            customerServiceInfoMapper.insert(customerServiceInfo);
        }else {
            CustomerServiceInfo customerServiceInfo = customerServiceInfos.get(0);
            customerServiceInfo.setCustomerServiceHotline(hotLine);
            customerServiceInfo.setEMail(eMail);
            customerServiceInfo.setUpdateId((long)managerId);
            customerServiceInfo.setUpdateTime(LocalDateTime.now());
            customerServiceInfoMapper.updateById(customerServiceInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<ContentDTO> queryContentClassification(Integer id) {
        ContentClassification contentClassification = contentClassificationMapper.selectById(id);
        ContentDTO contentDTO = new ContentDTO();
        BeanUtils.copyProperties(contentClassification,contentDTO);
        LocalDateTime updateTime = contentClassification.getUpdateTime();
        Long userId = contentClassification.getUpdateUid();
        if(updateTime == null){
            updateTime = contentClassification.getCreateTime();
            userId = contentClassification.getCreateUid();
        }
        if(updateTime!=null&&userId!=null){
            String time = DateUtil.convertDateToString(updateTime);
            PlatformManagers platformManagers = managersMapper.selectById(userId);
            contentDTO.setUpdateName(platformManagers.getNickname());
            contentDTO.setUpdateTime(time);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,contentDTO);
    }
}
