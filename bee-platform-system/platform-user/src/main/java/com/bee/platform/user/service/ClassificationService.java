package com.bee.platform.user.service;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.ContentClassification;
import com.bee.platform.user.entity.PrimaryClassification;
import com.bee.platform.user.entity.SecondaryClassification;
import io.swagger.models.auth.In;

import java.util.List;

/**
 * @ClassName: ClassificationService
 * @Description: 系统帮助 分类 service
 * @Author: fei.sun
 * @Date: 2019/4/28 10:25
 * @Version: 1.0
 */
public interface ClassificationService {

    ResponseResult deletePrimaryClassify(Integer userId,Integer id);

    ResponseResult queryPrimaryClassifyList(Integer classifyType);

    ResponseResult<HelperSubDTO> queryHelperClassifyInfo(Integer id);

    ResponseResult<List<SecondaryClassification>> queryHelperSecondClassify();

    ResponseResult<List<ContentClassification>> queryHelperContent(Integer id);

    ResponseResult deleteHelperSecondaryClassify(Integer userId,Integer id);

    ResponseResult<EditClassifyDTO> queryEditClassifyInfo(Integer id);

    ResponseResult submitPrimaryClassify(Integer userId,Integer id,String name,Integer weights,Integer classifyType);

    ResponseResult updateOrSaveSecondClassify(Integer userId,Integer id, String name, Integer weights,Integer classifyType,Integer pId);

    ResponseResult deleteSecondClassify(Integer userId,Integer id);

    void saveOrUpdateContent(Integer userId,ContentDTO contentDTO);

    void deleteContent(Integer userId,Integer id);

    ResponseResult<SecondClassifyListDTO> querySecondClassifyInfo(Integer id);

    ResponseResult submitHelperSecondClassify(Integer userId,Integer pId,Integer sId,Integer cId,Integer weights);

    ResponseResult<CustomerServiceDTO> queryCustomerServiceInfo();

    ResponseResult updateCustomerServiceInfo(Integer managerId, String hotLine, String eMail);

    ResponseResult<ContentDTO> queryContentClassification(Integer id);
}
