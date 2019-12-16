package com.bee.platform.user.email;

import cn.hutool.extra.template.Template;
import cn.hutool.extra.template.TemplateConfig;
import cn.hutool.extra.template.TemplateEngine;
import cn.hutool.extra.template.TemplateUtil;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.enums.FreeMarkerType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

/**
 * @description: FreeMarker模板工厂
 * @author: junyang.li
 * @create: 2019-05-21 15:07
 **/
@Slf4j
public class FreeMarkerFactory {
    /**
     *  FreeMarker工厂对象
     */
    private final static FreeMarkerFactory FREE_MARKER_FACTORYR=new FreeMarkerFactory();
    /**
     * 模板容器
     */
    private static Map<Integer,Template> templateMap=new HashMap<>(16);

    private static TemplateEngine engine;

    static {
        engine=TemplateUtil.createEngine(new TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH));
    }

    private FreeMarkerFactory() {
    }

    public static FreeMarkerFactory getFreeMarkerFactory(){
        return FREE_MARKER_FACTORYR;
    }
    /**
     * @notes: 获得模板并拼装数据
     * @Author: junyang.li
     * @Date: 15:27 2019/5/21
     * @param type : 模板类型
     * @param map : 参数列表
     * @return: java.lang.String
     */
    public String render(FreeMarkerType type, Map<String,Object> map){
        Template template=templateMap.get(type.getKey());
        if(template==null){
            template=getTemplate(type);
            if (template==null){
                log.error("无法从template文件夹中获取邮件模板{}",type.getName());
                throw new BusinessException(ResCodeEnum.BUSY_SYSTEM, ExceptionMessageEnum.EMAIL_TEMPLATE_NOT_FOUND);
            }
        }
        // map中的key，对应模板中的${key}表达式
        return template.render(map);
    }
    /**
     * @notes: 根据模板名称获得模板对象
     * @Author: junyang.li
     * @Date: 15:27 2019/5/21
     * @param type : 模板类型
     * @return: cn.hutool.extra.template.Template
     */
    private Template getTemplate(FreeMarkerType type){
        Template template= engine.getTemplate(type.getName());
        templateMap.put(type.getKey(),template);
        return template;
    }
}
