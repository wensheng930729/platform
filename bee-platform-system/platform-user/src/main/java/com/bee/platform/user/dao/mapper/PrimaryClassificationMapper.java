package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.dto.CustomerServiceDTO;
import com.bee.platform.user.dto.EditSecondClassifyDTO;
import com.bee.platform.user.dto.HelperContentDTO;
import com.bee.platform.user.dto.PrimaryClassifyListDTO;
import com.bee.platform.user.entity.PrimaryClassification;

import java.util.List;

/**
 * @ClassName: ClassificationMapper
 * @Description: 系统帮助 分类 mapper
 * @Author: fei.sun
 * @Date: 2019/4/28 10:21
 * @Version: 1.0
 */
public interface PrimaryClassificationMapper extends BaseMapper<PrimaryClassification> {

    List<PrimaryClassifyListDTO> queryPrimaryClassifyList(Integer classifyType);

    List<HelperContentDTO> queryHelperSecondClassifyInfo(Integer id);

    List<EditSecondClassifyDTO> queryEditClassifyInfo(Integer id);

    List<PrimaryClassifyListDTO> queryHelperPrimaryClassifyList();

    List<CustomerServiceDTO> queryCustomerServiceInfo();
}
