package com.bee.platform.user.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.entity.EnterprisesAttachment;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import com.bee.platform.user.utils.ValidList;
import org.springframework.validation.BindingResult;

import java.util.List;

/**
 * <p>
 * 企业附件信息表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-04-25
 */
public interface EnterprisesAttachmentService extends IService<EnterprisesAttachment> {

    /**
     * 保存企业附件
     * @param userInfo 用户信息
     * @param rqs 附件信息
     * @return  附件id
     */
    boolean saveEnterprisesAttachment(AuthPlatformUserInfo userInfo, List<EnterprisesAttachmentRQ> rqs);
}
