package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.entity.SecondaryClassification;

import java.util.List;

/**
 * @ClassName: SecondaryClassificationMapper
 * @Description: 系统帮助下 小类 mapper
 * @Author: fei.sun
 * @Date: 2019/4/28 14:44
 * @Version: 1.0
 */
public interface SecondaryClassificationMapper extends BaseMapper<SecondaryClassification> {
    List<SecondaryClassification> queryHelperSecondClassify();
}
